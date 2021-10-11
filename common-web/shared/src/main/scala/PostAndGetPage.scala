package ltbs.uniform
package common.web

import cats.implicits._
import concurrent.Future
import scala.concurrent.ExecutionContext
import validation._


trait PostAndGetPage[Html, T, A] extends WebInteraction[Html, T, A] {

  def codec: Codec[A]

  val customRouting: PartialFunction[List[String], A] = Map.empty

  def getPage(
    key: List[String],
    tell: Option[T],
    state: DB,
    existing: Input,
    breadcrumbs: Breadcrumbs,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Option[Html]

  def postPage(
    key: List[String],
    tell: Option[T],
    state: DB,
    request: Input,
    errors: ErrorTree,
    breadcrumbs: Breadcrumbs,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Option[Html]


  def trackLeapPoint(pageIn: PageIn[Html], id: String): (PageIn[Html], Boolean) = {
    import pageIn._

    (
      queryParams.get("leap-to"),
      state.get("_leap-from"::Nil).map(_.split("/").toList),
      state.get("_leap-to"::Nil).map(_.split("/").toList)
    ) match {

      case (_, Some(from), Some(to)) if from =!= to => // applying leap ahead
        // the first page after the from page that is not a subpage of the from page allows the journey to leap ahead to the 'to' page
        val currentId = pathPrefix :+ id
        val ret = breadcrumbs.exists(_.startsWith(from)) && !currentId.startsWith(from) && currentId =!= to && !breadcrumbs.contains(to)
        val newStateTwo = if (currentId === to || breadcrumbs.contains(to)) {
          (state - ("_leap-to"::Nil) ) - ("_leap-from"::Nil)
        } else state
        (pageIn.copy(state = newStateTwo), ret)

      case (Some(x :: Nil), _, _) if config.leapAhead =>  // setting leap ahead
        val newState = state + (
          ("_leap-to"   :: Nil) -> x,
          ("_leap-from" :: Nil) -> targetId.mkString("/")
        )
        (pageIn.copy(state = newState), false)

      case _ =>
        (pageIn, false)
    }
  }

  override def apply(
    id: String,
    tell: Option[T],
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])] = Map.empty
  ): WebMonad[Html, A] = new WebMonad[Html, A] {
    def apply(pageInPreLeapPoint: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, A]] = {
      val (pageIn, leapAhead) = trackLeapPoint(pageInPreLeapPoint, id)
      import pageIn.{messages => _, _}

      val messages = pageIn.messages.withCustomContent(customContent)
      val currentId = pageIn.pathPrefix :+ id

      lazy val dbInput: Option[Either[ErrorTree, Input]] =
        state.get(currentId).map{Input.fromUrlEncodedString}

      lazy val dbObject: Option[Either[ErrorTree,A]] = {
        val fromState = dbInput map {_ >>= codec.decode >>= validation.either}
        if (leapAhead) {
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
            val localData = post.atPath(id :: Nil)
            val parsed = (codec.decode(localData) >>= validation.either)

            parsed match {
              case Right(valid) =>
                pageIn.toPageOut(AskResult.Success[Html, A](valid)).copy (
                  breadcrumbs = currentId :: pageIn.breadcrumbs,
                  db = state + (currentId -> localData.toUrlEncodedString)
                ).pure[Future]
              case Left(error) =>
                val html = AskResult.Payload[Html, A](
                  postPage(id :: Nil, tell, state, localData, error, breadcrumbs, messages),
                  error,
                  messages
                )
                pageIn.toPageOut(html).copy(
                  breadcrumbs = currentId :: pageIn.breadcrumbs
                ).pure[Future]
            }

          case None =>
            val html = AskResult.Payload[Html, A](
              getPage(
                id :: Nil,
                tell,
                state,
                dbInput.flatMap{_.toOption} orElse            // db
                  default.map{x => codec.encode(x)} getOrElse // default
                  Input.empty,                                // neither
                breadcrumbs,
                messages
              ),
              ErrorTree.empty,
              messages
            )
            pageIn.toPageOut(html).copy(
              breadcrumbs =  currentId :: pageIn.breadcrumbs
            ).pure[Future]
        }
      } else if (targetIdP.startsWith(currentId) && customRouting.isDefinedAt(residual)) {
        val residualData = customRouting(residual)
        pageIn.toPageOut(AskResult.Success[Html, A](residualData)).copy(
          db = state + (currentId -> codec.encode(residualData).toUrlEncodedString)
        ).pure[Future]
      } else if (currentId.startsWith(targetIdP)) {
        Future.successful(pageIn.toPageOut(AskResult.GotoPath[Html,A](currentId)))
      } else {
        Future.successful{
          dbObject match {
            case Some(Right(data)) if targetId =!= Nil && targetId.lastOption =!= Some("") && (leapAhead || !breadcrumbs.contains(targetId)) =>
              // they're replaying the journey
              pageIn.toPageOut(AskResult.Success[Html,A](data)).copy(
                breadcrumbs = currentId :: pageIn.breadcrumbs
              )
            case _ =>
              pageIn.toPageOut(AskResult.GotoPath[Html,A](currentId))
          }
        }
      }
    }
  }
}
