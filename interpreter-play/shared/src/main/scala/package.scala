package ltbs.uniform.interpreters

import cats.data._
import cats.implicits._
import cats.Invariant
import play.api.data.Form
import play.api.mvc.{ Request, AnyContent }
import play.twirl.api.Html
import ltbs.uniform.datapipeline._

package object playframework {

  type Encoded = String
  type DB = Map[String,Encoded]
  type ValidationError = String
  type ValidatedData[A] = Option[Validated[ValidationError, A]]

  implicit val playFormFunctor: Invariant[Form] = new Invariant[Form]{
    def imap[A, B](fa: Form[A])(f: A => B)(g: B => A): Form[B] =
      new Form[B](fa.mapping.transform(f, g), fa.data, fa.errors, fa.value.map(f))
  }

  implicit val invariantWebMonad: Invariant[WebMonadForm] = new Invariant[WebMonadForm] {
    def imap[A, B](fa: WebMonadForm[A])(f: A => B)(g: B => A): WebMonadForm[B] = new WebMonadForm[B]{

      def render(key: String, existing: Input, errors: ErrorTree, breadcrumbs: List[String]): Html =
        fa.render(key, existing, errors, breadcrumbs)

      def fromRequest(key: String, request: Request[AnyContent]): Either[ErrorTree, B] =
        fa.fromRequest(key, request).map(f)
      def encode(in: B): Encoded = fa.encode(g(in))
      def decode(out: Encoded): Either[ErrorTree,B] = fa.decode(out).map(f)
      def toTree(in: B): Input = fa.toTree(g(in))

      def bind(in: Input): Either[Error,B] = fa.bind(in).map(f)
      def unbind(a: B): Input = fa.unbind(g(a))
    }
  }

  implicit val invariantWebMonadSelect: Invariant[WebMonadSelectPage] = new Invariant[WebMonadSelectPage] {

    def imap[A, B](fa: WebMonadSelectPage[A])(f: A => B)(g: B => A): WebMonadSelectPage[B] = new WebMonadSelectPage[B]{
      def toHtml(in: B): Html = fa.toHtml(g(in))
      def renderOne(key: String, options: Set[B], existing: ValidatedData[B], request: Request[AnyContent]): Html =
        fa.renderOne(key, options.map(g), existing.map{_.map(g)}, request)
      def renderMany(key: String, options: Set[B], existing: ValidatedData[Set[B]], request: Request[AnyContent]): Html =
        fa.renderMany(key, options.map(g), existing.map{_.map(_.map(g))}, request)
      def encode(in: B): Encoded = fa.encode(g(in))
      def decode(out: Encoded): B = f(fa.decode(out))
      def playFormOne(key: String, validation: B => Validated[ValidationError, B]): Form[B] =
        fa.playFormOne(key, { a: A => validation(f(a)).map(g) }).imap(f)(g)
      def playFormMany(key: String, validation: Set[B] => Validated[ValidationError, Set[B]]): Form[Set[B]] =
        fa.playFormMany(key, { a: Set[A] => validation(a.map(f)).map(_.map(g)) }).imap(_.map(f))(_.map(g))
    }
  }

  def inferWebMonadForm[A](chrome: (String, ErrorTree, Html, List[String]) => Html)
  (implicit
     parser: DataParser[A],
    html: HtmlForm[A],
    messages: Messages
  ): WebMonadForm[A] = new WebMonadForm[A] {

    def fromRequest(key: String,request: Request[AnyContent]): Either[ErrorTree,A] = {
      val inputText = request.body.asText.get
      parser.bind(decodeInput(inputText))
    }

    def render(key: String, input: Input, errors: ErrorTree, breadcrumbs: List[String]): Html =
      chrome(key, errors, html.render(key, input, errors, messages), breadcrumbs.reverse)

    def toTree(in: A): Input = parser.unbind(in)

    def decode(out: Encoded): Either[ErrorTree,A] = {
      val a = decodeInput(out)
      val b = a.get("root")
      b.flatMap{parser.bind(_)}
    }

    def encode(in: A): Encoded =
      encodeInput("root", parser.unbind(in))

    def bind(in: Input): Either[Error,A] = parser.bind(in)
    def unbind(a:A): Input = parser.unbind(a)

  }
}
