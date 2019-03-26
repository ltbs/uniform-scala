package ltbs.uniform

import cats.implicits._
import cats.arrow.Profunctor
import cats.{Invariant,Monoid}
import play.twirl.api.Html
import scala.language.implicitConversions

package object web {

  type FormUrlEncoded = Map[String, Seq[String]]
  type Input = Tree[String, List[String]]

  implicit val htmlMonoidInstance = new Monoid[Html] {
    def empty: Html = Html("")
    def combine(a: Html, b: Html):Html = Html(a.toString ++ b.toString)
  }

  implicit def richFormUrlEncoded(in: FormUrlEncoded): RichFormUrlEncoded =
    new RichFormUrlEncoded(in)

  implicit def sifInvariant[IN,OUT,TELL] = new Invariant[SimpleInteractionForm[IN,TELL,?,OUT]] {
    def imap[A, B](fa: SimpleInteractionForm[IN,TELL,A,OUT])(f: A => B)(g: B => A) =
      fa.transform(f map (_.asRight))(g)
  }

  def UrlEncodedHtmlForm[TELL,ASK](
    parser: DataParser[ASK],
    html: HtmlForm[ASK],
    renderTell: (TELL, String) => Html,
    messages: UniformMessages[Html]
  ): SimpleInteractionForm[FormUrlEncoded,TELL,ASK,Html] = { 
    val underlying = new InputHtmlForm(parser, html, renderTell, messages)
    underlying.transformIn(_.toInputTree)
  }

  protected[web] val required = "required"

}
