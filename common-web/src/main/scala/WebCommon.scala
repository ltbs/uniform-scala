import play.twirl.api.Html
import cats.arrow.Profunctor
import cats.Invariant
import cats.implicits._
import ltbs.uniform.datapipeline._

package ltbs.uniform.common {

  package object web {
    type Encoded = String
    type UrlEncoded = Map[String,List[String]]

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
  }

  package web {

    trait Dom
    trait Response

    trait SimpleInteractionForm[IN,A,OUT] {
      def render(key: String, existing: Option[Encoded], data: IN, errors: ErrorTree): OUT
      def render(key: String, existing: Option[Encoded], data: IN): OUT =
        render(key, existing, data, Tree.empty)
      def receiveInput(data: IN): Encoded
      def decodeInput(data: IN): Either[ErrorTree,A] = decode(receiveInput(data))
      def encode(in: A): Encoded
      def decode(out: Encoded): Either[ErrorTree,A]
        
      def transform[B](f: A => Either[ErrorTree,B])(g: B => A) = {
        val fa = this
        new SimpleInteractionForm[IN,B,OUT] {
          def decode(out: Encoded): Either[ErrorTree,B] = fa.decode(out).flatMap(f)
          def receiveInput(data: IN): Encoded = fa.receiveInput(data)
          def encode(in: B): Encoded = fa.encode(g(in))
          def render(key: String, existing: Option[Encoded], data: IN, errors: ErrorTree): OUT =
            fa.render(key,existing,data, errors)
        }
      }

      def validating(f: A => Either[ErrorTree,A]): SimpleInteractionForm[IN,A,OUT] =
        transform(f)(identity)
    }

    abstract class CommonForm[A] extends SimpleInteractionForm[UrlEncoded,A,Html] {
      override def render(key: String, existing: Option[Encoded], data: UrlEncoded, errors: ErrorTree): Html
      override def receiveInput(data: UrlEncoded): Encoded
      override def encode(in: A): Encoded // can be inferred
      override def decode(out: Encoded): Either[ErrorTree,A] // can be inferred

      def urlEncode(in: UrlEncoded): Encoded
      def urlDecode(out: Encoded): Either[ErrorTree, UrlEncoded]
    }

    trait JsForm[A] extends SimpleInteractionForm[Dom,A,Html] {
      override def render(key: String, existing: Option[Encoded], data: Dom, errors: ErrorTree): Html
      override def receiveInput(request: Dom): Encoded
      override def encode(in: A): Encoded
      override def decode(out: Encoded): Either[ErrorTree,A]
    }
  }
}
