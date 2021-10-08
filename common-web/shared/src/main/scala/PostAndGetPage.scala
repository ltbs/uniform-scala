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

  override def apply(
    id: String,
    tell: Option[T],
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])] = Map.empty
  ): WebMonad[Html, A] = new WebMonad[Html, A] {
    def apply(pageIn: PageIn[Html])(implicit ec: ExecutionContext): Future[PageOut[Html, A]] = {
      import pageIn.{messages => _, state => statePreLeap, _}

      val messages = pageIn.messages.withCustomContent(customContent)
      val currentId = pageIn.pathPrefix :+ id

      // should we try to leap ahead?
      val (state, leapAhead): (DB, Boolean) = {
        (
          pageIn.trackLeapPoint(statePreLeap).get("_leap-to"::Nil).map(_.split("/").toList),
          pageIn.trackLeapPoint(statePreLeap).get("_leap-from"::Nil).map(_.split("/").toList)
        ).mapN {
          case (to, from) =>

            println(s"breadcrumbs:${breadcrumbs}")
            println(s"from:${from.toList}")
            println(s"to:${to.toList}")
            println(s"currentId:$currentId")
            println(s"one:${breadcrumbs.exists(_.startsWith(from))}")
            println(s"two:${!currentId.startsWith(from)}")
            println(s"three:${currentId =!= to}")
            // the first page after the from page that is not a subpage of the from page allows the journey to leap ahead to the 'to' page
            val ret = breadcrumbs.exists(_.startsWith(from)) && !currentId.startsWith(from) && currentId =!= to && !breadcrumbs.contains(to)
            println(s"leapAhead:$ret")
            val newState = if (currentId === to || breadcrumbs.contains(to)) {
              println("UNSET LEAP AHEAD!")
              (statePreLeap - ("_leap-to"::Nil) ) - ("_leap-from"::Nil)
            } else statePreLeap
            (pageIn.trackLeapPoint(newState), ret)
        }.getOrElse((pageIn.trackLeapPoint(statePreLeap), false))
      }

      println(currentId + ": " + state.toString)

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
                  breadcrumbs = currentId :: pageIn.breadcrumbs,
                  db = state
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
              db = state,
              breadcrumbs =  currentId :: pageIn.breadcrumbs
            ).pure[Future]
        }
      } else if (targetIdP.startsWith(currentId) && customRouting.isDefinedAt(residual)) {
        val residualData = customRouting(residual)
        pageIn.toPageOut(AskResult.Success[Html, A](residualData)).copy(
          db = state + (currentId -> codec.encode(residualData).toUrlEncodedString)
        ).pure[Future]
      } else if (currentId.startsWith(targetIdP)) {
        println("HELL YEAH")
        Future.successful(pageIn.toPageOut(AskResult.GotoPath[Html,A](currentId)).copy(
          db = pageIn.trackLeapPoint(state)
        ))
      } else {
        Future.successful{
          dbObject match {
            case Some(Right(data)) if targetId =!= Nil && targetId.lastOption =!= Some("") && (leapAhead || !breadcrumbs.contains(targetId)) =>
              // they're replaying the journey
              pageIn.toPageOut(AskResult.Success[Html,A](data)).copy(
                db = state,
                breadcrumbs = currentId :: pageIn.breadcrumbs
              )
            case _ =>
              pageIn.toPageOut(AskResult.GotoPath[Html,A](currentId)).copy(db = state)
          }
        }
      }
    }
  }
}
