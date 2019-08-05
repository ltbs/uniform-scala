package ltbs.uniform
package common.web

import cats.Monad
import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext
import shapeless._

case class PageIn(
  config: JourneyConfig,
  currentId: List[String],
  targetId: List[String],
  path: Path,
  request: Option[Input],
  state: DB
)

trait WebMonad[+A,Html] {
  def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]]
}

// case class Router[A, Html](
//   table: Function[List[String], WebMonad[A, Html]]
// ) extends WebMonad[A, Html] {

//   def codec: FormFieldEncoding[A]
//   def validation: List[List[Rule[A]]]
//   def default: Option[A]

//   def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A,Html]] = {
//     import pageIn._

//     lazy val dbInput: Option[Either[ErrorTree, Input]] =
//       state.get(currentId).map{Input.fromUrlEncodedString}
//     lazy val dbObject: Option[Either[ErrorTree,A]] =
//       dbInput map {_ >>= codec.decode >>= validation.combined.either}

//     if (currentId.startsWith(targetId)) {
//       ???
//     } else {
//       dbObject match {
//         case Some(Right(data)) if targetId =!= Nil && !path.contains(targetId) =>
//           // they're replaying the journey
//           Future.successful(PageOut(currentId :: path, state, AskResult.Success(data)))
//         case _ =>
//           Future.successful(PageOut(path, state, AskResult.GotoPath(currentId)))
//       }
//     }

//   }
// }

trait PostAndGetPage[A, Html] extends WebMonad[A, Html] {

  def codec: FormFieldEncoding[A]
  def validation: List[List[Rule[A]]]
  def default: Option[A]

  def getPage(
    key: List[String],
    state: DB,
    existing: Input
  )(implicit ec: ExecutionContext): Future[PageOut[A, Html]]

  def postPage(
    key: List[String],
    state: DB,
    request: Input,
    errors: ErrorTree
  )(implicit ec: ExecutionContext): Future[PageOut[A, Html]]

  def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A, Html]] = {
    import pageIn._

    lazy val dbInput: Option[Either[ErrorTree, Input]] =
      state.get(currentId).map{Input.fromUrlEncodedString}
    lazy val dbObject: Option[Either[ErrorTree,A]] =
      dbInput map {_ >>= codec.decode >>= validation.combined.either}

    if (currentId == targetId) {

      request match {
        case Some(post) =>
          (codec.decode(post) >>= validation.combined.either) match {
            case Right(valid) =>
              Future.successful(PageOut(currentId :: path, state, AskResult.Success[A, Html](valid)))
            case Left(error) =>
              postPage(currentId, state, post, error)
          }

        case None =>
          getPage(
            currentId,
            state,
            existing = dbInput.flatMap{_.toOption} orElse // db
              default.map{x => codec.encode(x)} getOrElse // default 
              Input.empty                                 // neither
          )
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

object WebMonad {

  implicit def webMonadMonadInstance[Html] =
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
              case PageOut(_,db,AskResult.Success(a)) =>
                f(a).apply(pageIn.copy(state = db))
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

case class GenericWebInterpreter[
  SupportedTell <: HList,
  SupportedAsk  <: HList,
  Html
]()(
    implicit tellSummoner : TypeclassList[SupportedTell, GenericWebTell[?, Html]],
    askSummoner  : TypeclassList[SupportedAsk, GenericWebAsk[?, Html]]
) extends Language[WebMonad[?,Html], SupportedTell, SupportedAsk]{

  def interact[Tell, Ask](
    id: String,
    tell: Tell,
    defaultIn: Option[Ask],
    validationIn: List[List[Rule[Ask]]],
    customContent: Map[String,(String, List[Any])]
  )(implicit selectorTell: IndexOf[SupportedTell,Tell],
    selectorAsk: IndexOf[SupportedAsk,Ask]
  ): WebMonad[Ask,Html] = new PostAndGetPage[Ask, Html] {

    def codec: FormFieldEncoding[Ask] = ???

    def default: Option[Ask] = defaultIn

    def getPage(
      key: List[String],
      state: DB,
      existing: Input
    )(implicit ec: ExecutionContext): Future[PageOut[Ask,Html]] = ???

    def postPage(
      key: List[String],
      state: DB,
      request: Input,
      errors: ErrorTree
    )(implicit ec: ExecutionContext): Future[PageOut[Ask,Html]] = ???

    def validation: List[List[Rule[Ask]]] = validationIn

  }
}
