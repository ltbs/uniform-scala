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

  implicit def sifProfunctor[T] = new Profunctor[SimpleInteractionForm[?,T,?]] {
    def dimap[A, B, C, D](fab: SimpleInteractionForm[A,T,B])(f: C => A)(g: B => D) =
      new SimpleInteractionForm[C,T,D] {
        def decode(out: Encoded): Either[ErrorTree,T] = fab.decode(out)
        def receiveInput(data: C): Encoded = fab.receiveInput(f(data))
        def encode(in: T): Encoded = fab.encode(in)
        def render(key: String,existing: Option[Encoded], data: C, errors: ErrorTree): D =
          g(fab.render(key,existing,f(data), errors))
      }
  }

  implicit def sifInvariant[IN,OUT] = new Invariant[SimpleInteractionForm[IN,?,OUT]] {
    def imap[A, B](fa: SimpleInteractionForm[IN,A,OUT])(f: A => B)(g: B => A) =
      fa.transform(f map (_.asRight))(g)
  }

  def UrlEncodedHtmlForm[A](
    parser: DataParser[A],
    html: HtmlForm[A],
    messages: Messages
  ): SimpleInteractionForm[FormUrlEncoded,A,Html] = { 
    val underlying = new InputHtmlForm(parser, html, messages)
    sifProfunctor[A].lmap(underlying)(_.toInputTree)
  }

  protected[web] val required = "required"

}
