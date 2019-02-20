package ltbs.uniform.web

import ltbs.uniform._
import play.twirl.api.Html
import cats.implicits._

class InputHtmlForm[TELL,ASK](
  parser: DataParser[ASK],
  html: HtmlForm[ASK],
  renderTell: (TELL, String) => Html,
  messages: Messages
) extends SimpleInteractionForm[Input,TELL,ASK,Html] {

  def decode(out: Encoded): Either[ErrorTree,ASK] = 
    parser.bind(FormUrlEncoded.readString(out).toInputTree)

  def encode(in: ASK): Encoded = receiveInput(parser.unbind(in))
  def receiveInput(data: Input): Encoded = FormUrlEncoded.fromInputTree(data).writeString
  def render(key: String, tell: TELL, existing: Option[Encoded], data: Input, errors: ErrorTree): Html = {
    val populatedValues: Input = existing.fold(data){ 
      FormUrlEncoded.readString(_).toInputTree
    }
    html.render(key, populatedValues, errors, messages, renderTell(tell, key))
  }

}
