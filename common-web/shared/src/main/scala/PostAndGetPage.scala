package ltbs.uniform
package common.web

import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext

abstract class PostAndGetPage[A, Html: cats.Monoid] extends WebMonadConstructor[A, Html] {

  def codec: Codec[A]

  def getPage(
    key: List[String],
    state: DB,
    existing: Input,
    breadcrumbs: Breadcrumbs,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Html

  def postPage(
    key: List[String],
    state: DB,
    request: Input,
    errors: ErrorTree,
    breadcrumbs: Breadcrumbs,
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
      val currentId = pageIn.pathPrefix :+ id
      lazy val dbInput: Option[Either[ErrorTree, Input]] =
        state.get(currentId).map{Input.fromUrlEncodedString}

      lazy val dbObject: Option[Either[ErrorTree,A]] =
        dbInput map {_ >>= codec.decode >>= validation.combined.either}

      if (currentId === targetId) {
        println(s"MATCH: $currentId === ${targetId}")
        request match {
          case Some(post) =>
            println(s"posted")
            val localData = post.atPath(currentId)
            val parsed = (codec.decode(localData) >>= validation.combined.either)
            parsed match {
              case Right(valid) =>
                PageOut(currentId :: breadcrumbs, state + (currentId -> localData.toUrlEncodedString), AskResult.Success[A, Html](valid), pageIn.pathPrefix).pure[Future]
              case Left(error) =>
                PageOut(currentId :: breadcrumbs, state, AskResult.Payload[A, Html](
                  tell |+| postPage(currentId, state, localData, error, breadcrumbs, messages),
                  error,
                  messages
                ), pageIn.pathPrefix).pure[Future]
            }

          case None =>
            PageOut(currentId :: breadcrumbs, state, AskResult.Payload[A, Html](
              tell |+|
              getPage(
                currentId,
                state,
                dbInput.flatMap{_.toOption} orElse            // db
                  default.map{x => codec.encode(x)} getOrElse // default
                  Input.empty,                                // neither
                breadcrumbs,
                messages
              ),
              ErrorTree.empty,
              messages
            ), pageIn.pathPrefix).pure[Future]
        }
      } else {
        println(s"DIFFER: $currentId === ${targetId}")
        dbObject match {
          case Some(Right(data)) if targetId =!= Nil && !breadcrumbs.contains(targetId) =>
            // they're replaying the journey
            Future.successful(PageOut(currentId :: breadcrumbs, state, AskResult.Success(data), pageIn.pathPrefix))
          case _ =>
            Future.successful(PageOut(breadcrumbs, state, AskResult.GotoPath(currentId), pageIn.pathPrefix))
        }
      }
    }
  }
}

object PostAndGetPage {

  def apply[A,Html: cats.Monoid](
    fieldIn: FormField[A, Html]
  ): WebMonadConstructor[A, Html] = new PostAndGetPage[A, Html] {
    def codec: Codec[A] = fieldIn

    def getPage(
      key: List[String],
      state: DB,
      existing: Input,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Html =
      fieldIn.render(key, breadcrumbs, existing, ErrorTree.empty, messages)

    def postPage(
      key: List[String],
      state: DB,
      request: Input,
      errors: ErrorTree,
      breadcrumbs: Breadcrumbs,
      messages: UniformMessages[Html]
    )(implicit ec: ExecutionContext): Html =
      fieldIn.render(key, breadcrumbs, request, errors, messages)
  }

}
