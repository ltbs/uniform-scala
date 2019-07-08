package ltbs.uniform
package common.web

import concurrent.Future

class SimpleForm[A, Html](field: FormField[A, Html]) extends GenericWebAsk[A, Html] {

  def page(
    currentId: List[String], // the current step in the journey
    targetId: List[String], // the uri the user agent is asking for
    default: Option[A],
    validation: List[List[Rule[A]]],
    config: JourneyConfig,
    submittedData: Option[Input],
    path: Path, // previous steps, maybe this should be a chain?
    db: DB,
    messages: UniformMessages[Html]
  ): Future[PageOut[A,Html]] = {
    import cats.implicits._
    lazy val rawDbData: Option[String] = db.get(currentId)
    lazy val dbObject: Option[Either[ErrorTree,A]] =
      rawDbData map {Input.fromUrlEncodedString(_) >>= field.decode >>= validation.combined.either}

    val localData = targetId.lastOption match {
      case Some(key) => submittedData.map(_.subTree(key))
      case None => submittedData
    }

    if (targetId === currentId) {
      localData match {
        case None =>
          val errors: ErrorTree = dbObject match {
            case Some(Left(e)) => e
            case _ => ErrorTree.empty
          }

          val prepopulatedData: Option[Input] =
            rawDbData
              .map(Input.fromUrlEncodedString)
              .flatMap(_.toOption)
              .orElse(default.map{x => field.encode(x)})

          val result = field.render(currentId, path, prepopulatedData, errors, messages)
          Future.successful(PageOut(path, db, AskResult.Payload(result, errors)))
        case Some(rawPostData) =>
          val postObject: Either[ErrorTree,A] =
            field.decode(rawPostData) >>= validation.combined.either

          postObject match {
            case Left(errors) =>
              val result = field.render(currentId, path, Some(rawPostData), errors, messages)
              Future.successful(PageOut(path, db, AskResult.Payload(result, errors)))
            case Right(o) =>
              Future.successful(PageOut(currentId :: path, db + (currentId -> rawPostData.toUrlEncodedString), AskResult.Success(o)))
          }
      }
    } else {
      dbObject match {
        case Some(Right(data)) if targetId =!= Nil && !path.contains(targetId) =>
          // they're replaying the journey
          Future.successful(PageOut(currentId :: path, db, AskResult.Success(data)))
        case _ =>
          Future.successful(PageOut(path, db, AskResult.GotoPath(currentId)))
      }
    }
  }

}
