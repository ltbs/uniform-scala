package controllers

import play.api.i18n._
import play.api.mvc._
import ltbs.uniform.datapipeline.Messages
import scala.io.Source.fromFile
import cats.implicits._

class CmsController extends InjectedController {

  def file = new java.io.File("cmsserver/conf/messages")
  def messages: Messages[String] = {
    val content = fromFile(file).getLines.mkString("\n")
    Messages.fromPlayFormat(content)
  }

  def lookup(key: String) = {
    Action {
      messages.get(key.split("/").toList) match {
        case Some(x) => Ok(x).withHeaders("Access-Control-Allow-Origin" -> "*")
        case None => NotFound.withHeaders("Access-Control-Allow-Origin" -> "*")
      }
    }
  }

  def update(key: String) = {
    Action { implicit request =>
      val formData = request.body.asFormUrlEncoded
      val newValO = formData >>= {_.get("value")} >>= {_.headOption}
      newValO match {
        case Some(newVal) =>
          val update = getUpdatedData(key, newVal)
          new java.io.PrintWriter(file) {
            write(update.mkString("\n")); close
          }
          Status(200).withHeaders("Access-Control-Allow-Origin" -> "*")
        case None =>
          println(s"KEY: $key")          
          println(s"FORMDATA: $formData")
          NotFound.withHeaders("Access-Control-Allow-Origin" -> "*")
      }
    }
  }

  def getUpdatedData(
    key: String,
    newValue: String
  ): List[String] = {
    val (pre, after) =
      fromFile(file).getLines.toList span (!_.startsWith(s"$key="))

    val afterF = after.drop(1).dropWhile{x => !x.matches("^[a-zA-Z0-9_.-]*=.*$")}
    pre ++ (s"$key=$newValue" :: afterF)
  }

}
