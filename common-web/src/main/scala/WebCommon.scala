package ltbs.uniform.common.web

import play.twirl.api.Html

import cats.implicits._
import ltbs.uniform.datapipeline._

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

trait JsForm[A] extends SimpleInteractionForm[Dom,A,Html] {
  override def render(key: String, existing: Option[Encoded], data: Dom, errors: ErrorTree): Html
  override def receiveInput(request: Dom): Encoded
  override def encode(in: A): Encoded
  override def decode(out: Encoded): Either[ErrorTree,A]
}


