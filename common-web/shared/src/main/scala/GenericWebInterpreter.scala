package ltbs.uniform
package common.web

import cats.{Monad, Monoid}
import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext
import shapeless._

case class PageIn(
  targetId: List[String],
  path: Path,
  request: Option[Input],
  state: DB
)

abstract class WebMonad[A,Html: Monoid] {
  def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]]
}

trait WebMonadConstructor[A, Html] {
  def apply(
    id: String,
    tell: Html,
    defaultIn: Option[A],
    validationIn: List[List[Rule[A]]],
    messages: UniformMessages[Html]
  ): WebMonad[A, Html]
}

abstract class PostAndGetPage[A, Html: Monoid] extends WebMonadConstructor[A, Html] {

  def codec: FormFieldEncoding[A]

  def getPage(
    key: List[String],
    state: DB,
    existing: Input,
    path: Path,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Html

  def postPage(
    key: List[String],
    state: DB,
    request: Input,
    errors: ErrorTree,
    path: Path,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Html

  def apply(
    id: String,
    tell: Html,
    default: Option[A],
    validation: List[List[Rule[A]]],
    messages: UniformMessages[Html]
  ): WebMonad[A, Html] = new WebMonad[A, Html] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A, Html]] = {
      import pageIn._
      val currentId = List(id) // no subjourneys at present
      lazy val dbInput: Option[Either[ErrorTree, Input]] =
        state.get(currentId).map{Input.fromUrlEncodedString}

      lazy val dbObject: Option[Either[ErrorTree,A]] =
        dbInput map {_ >>= codec.decode >>= validation.combined.either}

      if (currentId === targetId) {

        request match {
          case Some(post) =>
            val localData = post / id
            val parsed = (codec.decode(localData) >>= validation.combined.either)
            parsed match {
              case Right(valid) =>
                PageOut(currentId :: path, state + (currentId -> localData.toUrlEncodedString), AskResult.Success[A, Html](valid)).pure[Future]
              case Left(error) =>
                PageOut(currentId :: path, state, AskResult.Payload[A, Html](
                  postPage(currentId, state, localData, error, path, messages),
                  error
                )).pure[Future]
            }

          case None =>
            PageOut(currentId :: path, state, AskResult.Payload[A, Html](
              getPage(
                currentId,
                state,
                dbInput.flatMap{_.toOption} orElse            // db
                  default.map{x => codec.encode(x)} getOrElse // default
                  Input.empty,                                // neither
                path,
                messages
              ),
              ErrorTree.empty
            )).pure[Future]
        }
      } else {
        dbObject match {
          case Some(Right(data)) if targetId =!= Nil && !path.contains(targetId) =>
            // they're replaying the journey
            Future.successful(PageOut(currentId :: path, state, AskResult.Success(data)))
          case _ =>
            Future.successful(PageOut(path, state, AskResult.GotoPath(currentId)))
        }
      }
    }
  }

}

object WebMonad {

  implicit def webMonadMonadInstance[Html: Monoid] =
    new Monad[WebMonad[?, Html]] {

      def pure[A](x: A): WebMonad[A,Html] = new WebMonad[A, Html] {
        def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]] = {
          import pageIn._
          PageOut(path,state,AskResult.Success[A,Html](x)).pure[Future]
        }
      }

      def flatMap[A, B](fa: WebMonad[A,Html])(f: A => WebMonad[B,Html]): WebMonad[B,Html] = {
        new WebMonad[B,Html] {
          def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[B,Html]] = {
            fa.apply(pageIn).flatMap[PageOut[B,Html]] { _ match {
              case PageOut(p,db,AskResult.Success(a)) =>
                f(a).apply(pageIn.copy(state = db, path = p))
              case PageOut(p,db,gp: AskResult.GotoPath[A, Html]) =>
                PageOut(p,db,gp.map[B]).pure[Future]
              case PageOut(p,db,pl: AskResult.Payload[A, Html]) =>
                PageOut(p,db,pl.map[B]).pure[Future]
            } }
          }
        }
      }

      // may not be stack-safe
      def tailRecM[A, B](a: A)(f: A => WebMonad[Either[A, B], Html]): WebMonad[B, Html] = flatMap(f(a)) {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => pure(b)
      }
    }
}

case class InterpreterFactory[
  Html: Monoid
]() {

  type WebTell[A] = GenericWebTell[A, Html]
  type WM[A] = WebMonadConstructor[A, Html]

  case class GenericWebInterpreter[
    SupportedTell <: HList,
    SupportedAsk  <: HList
  ](messages: UniformMessages[Html])(
    implicit tellSummoner : TypeclassList[SupportedTell, WebTell],
    webMonadSummoner      : TypeclassList[SupportedAsk, WM]
  ) extends Language[WebMonad[?,Html], SupportedTell, SupportedAsk]{
    def interact[Tell, Ask](
      id: String,
      tell: Tell,
      defaultIn: Option[Ask],
      validationIn: List[List[Rule[Ask]]],
      customContent: Map[String,(String, List[Any])]
    )(implicit selectorTell: IndexOf[SupportedTell,Tell],
      selectorAsk: IndexOf[SupportedAsk,Ask]
    ): WebMonad[Ask,Html] = {
      val customMessages = messages withCustomContent customContent
      val tellHtml = tellSummoner.forType[Tell].render(tell, id, customMessages)
      webMonadSummoner.forType[Ask].apply(
        id,
        tellHtml,
        defaultIn,
        validationIn,
        customMessages
      )
    }
  }

}
