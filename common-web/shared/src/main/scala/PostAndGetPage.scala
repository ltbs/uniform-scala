package ltbs.uniform
package common.web

import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext

abstract class PostAndGetPage[A, Html: cats.Monoid] extends WebMonadConstructor[A, Html] {

  def stats: FormFieldStats

  def codec: Codec[A]

  val customRouting: PartialFunction[List[String], A] = Map.empty

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
        dbInput map {_ >>= codec.decode >>= validation.combined.either} orElse
          default.map(x => validation.combined.either(x))

      lazy val residual = targetId.drop(currentId.size)
      if (targetId === currentId) {
        request match {
          case Some(post) =>
            val localData = post.atPath(currentId)
            val parsed = (codec.decode(localData) >>= validation.combined.either)
            parsed match {
              case Right(valid) =>
                PageOut(currentId :: breadcrumbs, state + (currentId -> localData.toUrlEncodedString), AskResult.Success[A, Html](valid), pageIn.pathPrefix).pure[Future]
              case Left(error) =>
                PageOut(currentId :: breadcrumbs, state, AskResult.Payload[A, Html](
                  tell |+| postPage(currentId, state, localData, error, breadcrumbs, messages),
                  error,
                  messages,
                  stats
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
              messages,
              stats
            ), pageIn.pathPrefix).pure[Future]
        }
      } else if (targetId.startsWith(currentId) && customRouting.isDefinedAt(residual)) {
        val residualData = customRouting(residual)
        Future.successful(
          PageOut(
            breadcrumbs,
            state + (currentId -> codec.encode(residualData).toUrlEncodedString),
            AskResult.Success(residualData),
            pageIn.pathPrefix
          )
        )
      } else {
        Future.successful(
          dbObject match {
            case Some(Right(data)) if targetId =!= Nil && !breadcrumbs.contains(targetId) =>
              // they're replaying the journey
              PageOut(currentId :: breadcrumbs, state, AskResult.Success(data), pageIn.pathPrefix)
            case _ =>
              PageOut(breadcrumbs, state, AskResult.GotoPath(currentId), pageIn.pathPrefix)
          }
        )
      }
    }
  }
}

class SimplePostAndGetPage[A,Html: cats.Monoid](
  fieldIn: FormField[A, Html]
) extends PostAndGetPage[A, Html] {

    def stats: FormFieldStats = fieldIn.stats

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

object PostAndGetPage {

  def apply[A,Html: cats.Monoid](
    fieldIn: FormField[A, Html]
  ): WebMonadConstructor[A, Html] = new SimplePostAndGetPage[A, Html](fieldIn)

}
