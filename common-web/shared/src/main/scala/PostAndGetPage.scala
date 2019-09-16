package ltbs.uniform
package common.web

import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext

abstract class PostAndGetPage[A, Html] extends WebMonadConstructor[A, Html] {

  def isCompound: Boolean

  def codec: Codec[A]

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
        default.fold(dbInput map {_ >>= codec.decode >>= validation.combined.either}){
          x => validation.combined.either(x).some
        }

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
                  error,
                  messages,
                  isCompound
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
              ErrorTree.empty,
              messages,
              isCompound
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

object PostAndGetPage {

  def apply[A,Html](
    fieldIn: FormField[A, Html]
  ): WebMonadConstructor[A, Html] = new PostAndGetPage[A, Html] {

    def isCompound: Boolean = fieldIn.isCompound

    def codec: Codec[A] = fieldIn

    def getPage(
      key: List[String],
      state: DB,
      existing: Input,
      path: Path,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Html =
      fieldIn.render(key, path, existing, ErrorTree.empty, messages)

    def postPage(
      key: List[String],
      state: DB,
      request: Input,
      errors: ErrorTree,
      path: Path,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Html =
      fieldIn.render(key, path, request, errors, messages)
  }

}
