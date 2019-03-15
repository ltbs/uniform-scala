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

  implicit def sifProfunctor[TELL,ASK] = new Profunctor[SimpleInteractionForm[?,TELL,ASK,?]] {
    def dimap[A, B, C, D](fab: SimpleInteractionForm[A,TELL,ASK,B])(f: C => A)(g: B => D) =
      new SimpleInteractionForm[C,TELL,ASK,D] {
        def decode(out: Encoded): Either[ErrorTree,ASK] = fab.decode(out)
        def receiveInput(data: C): Encoded = fab.receiveInput(f(data))
        def encode(in: ASK): Encoded = fab.encode(in)
        def render(key: String, tell: TELL, existing: Option[Encoded], data: C, errors: ErrorTree): D =
          g(fab.render(key, tell, existing,f(data), errors))
      }
  }

  implicit def sifInvariant[IN,OUT,TELL] = new Invariant[SimpleInteractionForm[IN,TELL,?,OUT]] {
    def imap[A, B](fa: SimpleInteractionForm[IN,TELL,A,OUT])(f: A => B)(g: B => A) =
      fa.transform(f map (_.asRight))(g)
  }

  def UrlEncodedHtmlForm[TELL,ASK](
    parser: DataParser[ASK],
    html: HtmlForm[ASK],
    renderTell: (TELL, String) => Html,
    messages: Messages
  ): SimpleInteractionForm[FormUrlEncoded,TELL,ASK,Html] = { 
    val underlying = new InputHtmlForm(parser, html, renderTell, messages)
    sifProfunctor[TELL,ASK].lmap(underlying)(_.toInputTree)
  }

  protected[web] val required = "required"


  implicit val messagesMonoidInstance = new Monoid[Messages] {
    def empty: Messages = NoopMessages
    def combine(a: Messages, b: Messages):Messages = new Messages {
      def get(key: String, args: Any*): Option[String] = a.get(key, args).orElse(b.get(key, args))
      def get(key: List[String], args: Any*): Option[String] = a.get(key, args).orElse(b.get(key, args))
      def list(key: String, args: Any*): List[String] = a.list(key, args) |+| b.list(key, args)
    }
  }

}
