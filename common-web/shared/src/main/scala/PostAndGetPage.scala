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
    pageIn: PageIn[Html], 
    key: List[String],
    tell: Option[T],
    existing: Input, 
    rule: Rule[A]
  )(implicit ec: ExecutionContext): Option[Html]

  def postPage(
    pageIn: PageIn[Html],     
    key: List[String],
    tell: Option[T],
    request: Input,
    rule: Rule[A],    
    errors: ErrorTree
  )(implicit ec: ExecutionContext): Option[Html]

  def trackLeapPoint(pageIn: PageIn[Html], id: String): (PageIn[Html], Boolean) = {
    import pageIn._

    (
      queryParams.get("leap-to"),
      leapPoints
    ) match {

      case (_, Some((from, to))) if from =!= to => // applying leap ahead
        // the first page after the from page that is not a subpage of the from page allows the journey to leap ahead to the 'to' page
        val currentId = pathPrefix :+ id
        val ret = breadcrumbs.exists(_.startsWith(from)) && !currentId.startsWith(from) && currentId =!= to && !breadcrumbs.contains(to)
        val newStateTwo = if (currentId === to || breadcrumbs.contains(to)) {
          (state - ("_leap-to"::Nil) ) - ("_leap-from"::Nil)
        } else state
        (pageIn.copy(state = newStateTwo), ret)

      case (Some(x :: Nil), _) if config.leapAhead =>  // setting leap ahead
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
//      import pageIn.{messages => _, _}

      val messages = pageIn.messages.withCustomContent(customContent)
      val currentId = pageIn.pathPrefix :+ id

      lazy val dbInput: Option[Either[ErrorTree, Input]] =
        pageIn.state.get(currentId).map{Input.fromUrlEncodedString}

      lazy val dbObject: Option[Either[ErrorTree,A]] = {
        val fromState = dbInput map {_ >>= codec.decode >>= validation.either}
        if (leapAhead) {
          fromState orElse default.map(validation.either)
        } else {
          fromState
        }
      }

      // we need to ignore cases with a trailing slash
      val targetIdP = pageIn.targetId.reverse.dropWhile(_ == "").reverse

      import pageIn.forceContinuation

      lazy val residual = targetIdP.drop(currentId.size)
      if (targetIdP === currentId && !forceContinuation) {
        pageIn.request match {
          case Some(post) =>
            val localData = post.atPath(id :: Nil)
            val parsed = (codec.decode(localData) >>= validation.either)

            parsed match {
              case Right(valid) =>
                pageIn.toPageOut(AskResult.Success[Html, A](valid)).copy (
                  breadcrumbs = currentId :: pageIn.breadcrumbs,
                  db = pageIn.state + (currentId -> localData.toUrlEncodedString)
                ).pure[Future]
              case Left(error) =>
                val html = AskResult.Payload[Html, A](
                  postPage(pageIn, id :: Nil, tell, localData, validation, error),
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
                pageIn, 
                id :: Nil,
                tell,
                dbInput.flatMap{_.toOption} orElse            // db
                  default.map{x => codec.encode(x)} getOrElse // default
                  Input.empty,                                // neither
                validation
              ),
              ErrorTree.empty,
              messages
            )
            pageIn.toPageOut(html).copy(
              breadcrumbs =  currentId :: pageIn.breadcrumbs
            ).pure[Future]
        }
      } else if (targetIdP.startsWith(currentId) && customRouting.isDefinedAt(residual) && !forceContinuation) {
        val residualData = customRouting(residual)
        pageIn.toPageOut(AskResult.Success[Html, A](residualData)).copy(
          db = pageIn.state + (currentId -> codec.encode(residualData).toUrlEncodedString)
        ).pure[Future]
      } else if (currentId.startsWith(targetIdP) && !forceContinuation) {
        Future.successful(pageIn.toPageOut(AskResult.GotoPath[Html,A](currentId)))
      } else {
        Future.successful{
          dbObject match {
            case Some(Right(data)) if pageIn.targetId =!= Nil && pageIn.targetId.lastOption =!= Some("") && (leapAhead || !pageIn.breadcrumbs.contains(pageIn.targetId)) =>
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
