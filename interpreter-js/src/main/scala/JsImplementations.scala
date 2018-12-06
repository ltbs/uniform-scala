package ltbs.uniform.prototype

import ltbs.uniform.{datapipeline => dpl}
import org.querki.jquery._
import JsInterpreter._
import cats.implicits._ // needed for monadic either in 2.11

object JsImplementations {

  def inferJsForm[A]
    (implicit
      parser: dpl.DataParser[A],
      html: dpl.HtmlForm[A]
    ): Form[A] = new Form[A] {

    def decode(out: Encoded): Either[ErrorTree,A] = {
      val a = dpl.decodeInput(out)
      val b = a.get("root")
      b.flatMap{parser.bind(_)}
    }

    def encode(in: A): Encoded =
      dpl.encodeInput("root", parser.unbind(in))

    def fromDataTree(key: String, datatree: dpl.Tree[String,List[String]]): Either[ErrorTree, A] = {
      datatree.get(key).flatMap(parser.bind)
    }

    def fromNode(key: String, fieldSet: JQuery): Either[ErrorTree,A] = {
      val fields = $("fieldset.uniform").serialize()
      println(s"raw data: $fields")
      val decoded=dpl.decodeUrlString(fields)
      val input = dpl.formToInput(decoded)
      parser.bind(input.children.getOrElse(key,dpl.Tree(Nil): dpl.Input))
    }

    def render(key: String, existing: Option[dpl.Input], errors: ErrorTree): String = {
      val values: dpl.Input = existing.getOrElse(dpl.Tree(Nil))

      s"""<fieldset id="uniform" class="uniform" style="border: 0px white; padding: 0px;">""" ++
      html.render(key, values, errors, dpl.NoopMessages).toString ++
      "</fieldset>"
    }
  }
}
