package ltbs.uniform.common

import cats.implicits._
import cats.arrow.Profunctor
import cats.Invariant
import ltbs.uniform.datapipeline._
import play.twirl.api.Html

package object web {
  type Encoded = String

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
    sifProfunctor[A].lmap(underlying)(underlying.formToInput)
  }

}
