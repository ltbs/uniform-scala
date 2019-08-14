package ltbs.uniform
package common.web

import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext
import shapeless._

trait WebMonadConstructor[A, Html] {
  def apply(
    id: String,
    tell: Html,
    defaultIn: Option[A],
    validationIn: List[List[Rule[A]]],
    messages: UniformMessages[Html]
  ): WebMonad[A, Html]
}

abstract class PostAndGetPage[A, Html] extends WebMonadConstructor[A, Html] {

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

trait GenericWebInterpreter[Html] {

  type WebTell[A] = GenericWebTell[A, Html]
  type WMC[A] = WebMonadConstructor[A, Html]
  type WM[A] = WebMonad[A, Html]

  def create[
    SupportedTell <: HList,
    SupportedAsk  <: HList
  ](messages: UniformMessages[Html])(
    implicit tellSummoner : TypeclassList[SupportedTell, WebTell],
    webMonadSummoner      : TypeclassList[SupportedAsk, WMC]
  ) = new Language[WebMonad[?,Html], SupportedTell, SupportedAsk]{
    def interact[Tell, Ask](
      id: String,
      tell: Tell,
      defaultIn: Option[Ask],
      validationIn: List[List[Rule[Ask]]],
      customContent: Map[String,(String, List[Any])]
    )(implicit
      selectorTell: IndexOf[SupportedTell,Tell],
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
