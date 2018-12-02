package ltbs.uniform.prototype

import cats.data.NonEmptyList
import scala.scalajs.runtime.UndefinedBehaviorError
import ltbs.uniform.{datapipeline => dpl}
import org.querki.jquery._
import JsInterpreter._
import cats.implicits._

object JsImplementations {

  def inferJsForm[A]
    (implicit
      parser: dpl.DataParser[A],
      html: dpl.HtmlForm[A]
    ): Form[A] = new Form[A] {

    def decode(out: Encoded): Either[ErrorTree,A] = {
      println(s"Decoding $out")
      val a = dpl.decodeInput(out)
      println(s"Decoding $out: a: $a")
      val b = a.get("root")
      println(s"Decoding $out: b: $b")
      val c = b.flatMap{parser.bind(_)}
      println(s"Decoding $out gave $c")
      c
    }

    def encode(in: A): Encoded = {
      println(s"Encoding $in")
      val e = dpl.encodeInput("root", parser.unbind(in))
      println(s"Encoding $in gave $e")
      e
    }

    def fromNode(key: String, fieldSet: JQuery): Either[ErrorTree,A] = {
      val fields = $("fieldset").serialize()
      println(s"fromNode fields: $fields")
      val decoded=dpl.decodeUrlString(fields)
      println(s"fromNode decoded: $decoded")
      val input = dpl.formToInput(decoded)
      println(s"fromNode input: $input")
      input.get(key).flatMap(parser.bind)
    }

    def render(key: String, existing: Option[A]): String = {
      val values: dpl.Input = existing.map{parser.unbind}.getOrElse(dpl.Tree(Nil))

      s"""<fieldset id="$key" class="form-field-group">""" ++
      html.render(key, values, dpl.Tree(""), dpl.NoopMessages).toString ++
      "</fieldset>"
    }
  }
}
