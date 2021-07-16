package ltbs.uniform
package interpreters

import play.api._
import play.twirl.api.{Html => TwirlHtml}

package object playframework extends common.web.webcommon {

  type Encoded = String

  implicit class RichPlayMessages(input: i18n.Messages) {

    def convertMessagesTwirlHtml(escapeHtml: Boolean = true): UniformMessages[TwirlHtml] = {
      val stringMessages = convertMessages()

      if (escapeHtml) stringMessages.map(
        play.twirl.api.HtmlFormat.escape
      ) else
        stringMessages.map(TwirlHtml.apply)    
    }

    def convertMessages() = new UniformMessages[String] {
        override def apply(key: List[String], args: Any*): String = {
          input(key, args: _*)
        }

        override def apply(key: String, args: Any*): String = {
          input(key, args: _*)
        }

        def get(key: String, args: Any*): Option[String] = if (input.isDefinedAt(key))
          Some(input.messages(key, args: _*))
        else
          None

        override def get(key: List[String], args: Any*): Option[String] = key collectFirst {
          case k if input.isDefinedAt(k) => input.messages(k, args: _*)
        }

        def list(key: String, args: Any*): List[String] = {
          @annotation.tailrec
          def inner(cnt: Int = 2, acc: List[String] = Nil): List[String] =
            get(s"$key.$cnt", args: _*) match {
              case Some(m) => inner(cnt + 1, m :: acc)
              case None => acc
            }

          List(key, s"$key.1").map(get(_, args: _*)).flatten ++ inner().reverse
        }
      }
  }

  implicit val mon: cats.Monoid[TwirlHtml] = new cats.Monoid[TwirlHtml] {
    def empty: TwirlHtml = TwirlHtml("")
    def combine(a: TwirlHtml, b: TwirlHtml) = TwirlHtml(a.toString + b.toString)
  }

}
