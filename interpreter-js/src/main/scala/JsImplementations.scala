package ltbs.uniform.prototype

import ltbs.uniform._
import ltbs.uniform.web._
import org.querki.jquery._
import JsInterpreter._
import cats.implicits._ // needed for monadic either in 2.11

object JsImplementations {

  def inferJsForm[A]
    (implicit
      parser: DataParser[A],
      html: HtmlForm[A],
      messages: Messages
    ): Form[A] = new Form[A] {

    def decode(out: Encoded): Either[ErrorTree,A] = {
      val a = FormUrlEncoded.readString(out).toInputTree
      parser.bind(a)
    }

    def encode(in: A): Encoded =
      FormUrlEncoded.fromInputTree(parser.unbind(in)).writeString

    def fromNode(key: String, fieldSet: JQuery): Either[ErrorTree,A] = {
      val fields = $("fieldset.uniform").serialize()
      println(s"raw data: $fields")
      val decoded=FormUrlEncoded.readString(fields)
      val input = decoded.toInputTree
      parser.bind(input.children.getOrElse(key,Tree(Nil): Input))
    }

    def toDataTree(in: A): Input =
      parser.unbind(in)

    def render(key: String, existing: Option[Input], errors: ErrorTree, tell: play.twirl.api.Html): String = {
      val values: Input = existing.getOrElse(Tree(Nil))

      s"""<fieldset id="uniform" class="uniform" style="border: 0px white; padding: 0px;">""" ++
      html.render(key, values, errors, messages, tell).toString ++
      "</fieldset>"
    }
  }
}
