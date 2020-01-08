package ltbs.uniform
package common.web

import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext
import validation._

abstract class PostAndGetPage[A, Html] extends WebMonadConstructor[A, Html] {

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
    validation: Rule[A],
    messages: UniformMessages[Html]
  ): WebMonad[A, Html] = new WebMonad[A, Html] {
    def apply(pageIn: PageIn)(implicit ec: ExecutionContext): Future[PageOut[A, Html]] = {
      import pageIn._
      val currentId = pageIn.pathPrefix :+ id
      lazy val dbInput: Option[Either[ErrorTree, Input]] =
        state.get(currentId).map{Input.fromUrlEncodedString}

      lazy val dbObject: Option[Either[ErrorTree,A]] = {
        val fromState = dbInput map {_ >>= codec.decode >>= validation.either}
        if (config.leapAhead) {
          fromState orElse default.map(validation.either)
        } else {
          fromState
        }
      }

      // we need to ignore cases with a trailing slash 
      val targetIdP = targetId.reverse.dropWhile(_ == "").reverse

      lazy val residual = targetIdP.drop(currentId.size)
      if (targetIdP === currentId) {
        request match {
          case Some(post) =>
            val localData = post.atPath(currentId)
            val parsed = (codec.decode(localData) >>= validation.either)

            parsed match {
              case Right(valid) =>
                pageIn.toPageOut(AskResult.Success[A, Html](valid)).copy (
                  breadcrumbs = currentId :: pageIn.breadcrumbs,
                  db = pageIn.state + (currentId -> localData.toUrlEncodedString)
                ).pure[Future]
              case Left(error) =>
                val html = AskResult.Payload[A, Html](
                  tell,
                  postPage(currentId, state, localData, error, breadcrumbs, messages),
                  error,
                  messages,
                  stats
                )
                pageIn.toPageOut(html).copy(
                  breadcrumbs = currentId :: pageIn.breadcrumbs
                ).pure[Future]
            }

          case None =>
            val html = AskResult.Payload[A, Html](
              tell,
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
            )
            pageIn.toPageOut(html).copy(
              breadcrumbs =  currentId :: pageIn.breadcrumbs
            ).pure[Future]
        }
      } else if (targetIdP.startsWith(currentId) && customRouting.isDefinedAt(residual)) {
        val residualData = customRouting(residual)
        pageIn.toPageOut(AskResult.Success[A, Html](residualData)).copy(
          db = state + (currentId -> codec.encode(residualData).toUrlEncodedString)
        ).pure[Future]
      } else {
        Future.successful{
          dbObject match {
            case Some(Right(data)) if targetId =!= Nil && targetId.lastOption =!= Some("") && !breadcrumbs.contains(targetId) =>
              // they're replaying the journey
              pageIn.toPageOut(AskResult.Success[A,Html](data)).copy(
                breadcrumbs = currentId :: pageIn.breadcrumbs
              )
            case _ =>
              pageIn.toPageOut(AskResult.GotoPath[A,Html](currentId))
          }
        }
      }
    }
  }
}

class SimplePostAndGetPage[A,Html](
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

  def apply[A,Html](
    fieldIn: FormField[A, Html]
  ): WebMonadConstructor[A, Html] = new SimplePostAndGetPage[A, Html](fieldIn)

}
