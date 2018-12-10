package ltbs.uniform.interpreters.playframework

import cats.data.Validated
import play.twirl.api.Html
import play.api.data.Form
import play.api.mvc.{ Request, AnyContent }
import ltbs.uniform.datapipeline._

trait WebMonadForm[T] {
  def render(key: String, existing: Input, errors: ErrorTree, breadcrumbs: List[String]): Html
  def fromRequest(key: String, request: Request[AnyContent]): Either[ErrorTree, T]
  def encode(in: T): Encoded
  def decode(out: Encoded): Either[ErrorTree,T]
  def toTree(in: T): Input

  def bind(in: Input): Either[Error,T]
  def unbind(a:T): Input
}

trait WebMonadSelectPage[T] {
  def toHtml(in: T): Html
  def renderOne(key: String, options: Set[T], existing: ValidatedData[T], request: Request[AnyContent]): Html
  def renderMany(key: String, options: Set[T], existing: ValidatedData[Set[T]], request: Request[AnyContent]): Html
  def encode(in: T): Encoded
  def decode(out: Encoded): T
  def playFormOne(key: String, validation: T => Validated[ValidationError, T]): Form[T]
  def playFormMany(key: String, validation: Set[T] => Validated[ValidationError, Set[T]]): Form[Set[T]]
}
