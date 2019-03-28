package ltbs.uniform.interpreters

import cats.data._
import cats.Invariant
import play.api.data.Form
import play.api.mvc.{ Request, AnyContent }
import play.twirl.api.{Html, HtmlFormat}
import ltbs.uniform._
import play.api._

package object playframework {

  type PlayForm[TELL,ASK] = SimpleInteractionForm[Request[AnyContent],TELL,ASK,Html]

  type Encoded = String

  type ValidationError = String
  type ValidatedData[A] = Option[Validated[ValidationError, A]]

  implicit val playFormFunctor: Invariant[Form] = new Invariant[Form]{
    def imap[A, B](fa: Form[A])(f: A => B)(g: B => A): Form[B] =
      new Form[B](fa.mapping.transform(f, g), fa.data, fa.errors, fa.value.map(f))
  }

  implicit class RichList[ELEM](inner: List[ELEM]) {
    def replace(ordinal: Int, elem: ELEM): List[ELEM] = 
      if (ordinal >= 0 && ordinal < inner.size) 
        inner.take(ordinal) ++ {elem :: inner.drop(ordinal + 1)}
      else
        throw new IndexOutOfBoundsException

    def delete(ordinal: Int): List[ELEM] =
      if (ordinal >= 0 && ordinal < inner.size) 
        inner.take(ordinal) ++ inner.drop(ordinal + 1)
      else
        throw new IndexOutOfBoundsException
  }

  implicit def renderTell: (Unit, String) => Html = {case _ => Html("")}

  def convertMessages(input: i18n.Messages, escapeHtml: Boolean = false): UniformMessages[Html] = {
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
    if (escapeHtml) stringMessages.map(HtmlFormat.escape) else stringMessages.map(Html.apply)
  }

}
