package ltbs.uniform
package interpreters

import cats.data._
import common.web.GenericWebTell
import play.api._, mvc.{ Request, Result, AnyContent }
import play.twirl.api.{Html => TwirlHtml}
import scala.concurrent.Future

package object playframework extends common.web.webcommon {

  type Encoded = String
  type WebInner[A] = RWST[Future, (JourneyConfig, List[String], Request[AnyContent]), Unit, (Path, DB), A]
  type WebMonad[A] = EitherT[WebInner, Result, A]

  type FormField[A,B] = common.web.FormField[A,B]

  implicit val tellTwirlUnit = new GenericWebTell[Unit,TwirlHtml] {
    def render(in: Unit, key: String, messages: UniformMessages[TwirlHtml]): TwirlHtml = TwirlHtml("")
  }

  implicit val twirlUnitField = new FormField[Unit,TwirlHtml] {
    def decode(out: Input): Either[ltbs.uniform.ErrorTree,Unit] = Right(())
    def encode(in: Unit): Input = Input.empty
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[TwirlHtml]
    ): TwirlHtml = TwirlHtml("")
  }


  implicit class RichTwirlInterpreter(interpreter: PlayInterpreter[TwirlHtml]) {
    def convertMessages(input: i18n.Messages, escapeHtml: Boolean = false): UniformMessages[TwirlHtml] = {
      val stringMessages = new UniformMessages[String]{
        override def apply(key: List[String],args: Any*): String = {
          input(key, args:_*)
        }
        override def apply(key: String,args: Any*): String = {
          input(key, args:_*)
        }
        def get(key: String,args: Any*): Option[String] = if (input.isDefinedAt(key))
          Some(input.messages(key, args:_*))
        else
          None

        override def get(key: List[String],args: Any*): Option[String] = key collectFirst {
          case k if input.isDefinedAt(k) => input.messages(k, args:_*)
        }

        def list(key: String,args: Any*): List[String] = {
          @annotation.tailrec
          def inner(cnt: Int = 2, acc: List[String] = Nil): List[String] =
            get(s"$key.$cnt", args:_*) match {
              case Some(m) => inner(cnt+1, m :: acc)
              case None    => acc
            }

          List(key, s"$key.1").map(get(_, args:_*)).flatten ++ inner().reverse
        }
      }
      if (escapeHtml) stringMessages.map(
        play.twirl.api.HtmlFormat.escape
      ) else
          stringMessages.map(TwirlHtml.apply)
    }
  }

  implicit val mon: cats.Monoid[TwirlHtml] = new cats.Monoid[TwirlHtml] {
    def empty: TwirlHtml = TwirlHtml("")
    def combine(a: TwirlHtml, b: TwirlHtml) = TwirlHtml(a.toString + b.toString)
  }

}
