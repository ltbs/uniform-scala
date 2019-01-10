package ltbs.uniform.web

import ltbs.uniform._
import play.twirl.api.Html
import cats.implicits._

class InputHtmlForm[A](
  parser: DataParser[A],
  html: HtmlForm[A],
  messages: Messages
) extends SimpleInteractionForm[Input,A,Html] {

  def decode(out: Encoded): Either[ErrorTree,A] = 
    parser.bind(FormUrlEncoded.readString(out).toInputTree)

  def encode(in: A): Encoded = receiveInput(parser.unbind(in))
  def receiveInput(data: Input): Encoded = FormUrlEncoded.fromInputTree(data).writeString
  def render(key: String, existing: Option[Encoded], data: Input, errors: ErrorTree): Html =
    html.render(key, data.prefix(key), errors, messages)

}
