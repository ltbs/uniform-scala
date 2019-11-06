package ltbs.uniform
package interpreters

import common.web._
import play.api._
import play.twirl.api.{Html => TwirlHtml}

package object playframework extends common.web.webcommon {

  type Encoded = String

  type WebMonad[A,Html] = common.web.WebMonad[A, Html]

  implicit val tellTwirlUnit = new GenericWebTell[Unit,TwirlHtml] {
    def render(in: Unit, key: String, messages: UniformMessages[TwirlHtml]): TwirlHtml = TwirlHtml("")
  }

  implicit val twirlUnitField = new FormField[Unit,TwirlHtml] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(
      key: List[String],
      breadcrumbs: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[TwirlHtml]
    ): TwirlHtml = TwirlHtml("")

  }

  implicit class RichPlayMessages(input: i18n.Messages) {

    def convertMessagesTwirlHtml(escapeHtml: Boolean = false): UniformMessages[TwirlHtml] = {
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

  implicit class RichTwirlInterpreter(interpreter: GenericWebInterpreter[TwirlHtml]) {
    @deprecated("Please use RichPlayMessages instead", "4.5.2")
    def convertMessages(input: i18n.Messages, escapeHtml: Boolean = false): UniformMessages[TwirlHtml] =
      RichPlayMessages(input).convertMessagesTwirlHtml(escapeHtml)
  }

  implicit val mon: cats.Monoid[TwirlHtml] = new cats.Monoid[TwirlHtml] {
    def empty: TwirlHtml = TwirlHtml("")
    def combine(a: TwirlHtml, b: TwirlHtml) = TwirlHtml(a.toString + b.toString)
  }

}
