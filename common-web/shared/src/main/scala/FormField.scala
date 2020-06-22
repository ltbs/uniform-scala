package ltbs.uniform
package common.web

import cats.syntax.either._
import cats.syntax.flatMap._
import scala.concurrent.ExecutionContext

/** Defines both the rendering and the encoding for a given datatype */
trait FormField[A, Html] extends Codec[A] with PostAndGetPage[A, Html]{

  def render(
    pageKey: List[String],
    fieldKey: List[String],    
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  /** Produce a new `FormField` from this one by mapping the types */
  override def imap[B](f: A => B)(g: B => A): FormField[B, Html] =
    simap[B](f(_).asRight)(g)

  /** Produce a new `FormField` from this one, with the
    * possibility of extending the validation
    */
  override def simap[B](f: A => Either[ErrorTree,B])(g: B => A): FormField[B, Html] = {
    val orig = this

    new FormField[B, Html] {
      def encode(in: B): Input = orig.encode(g(in))
      def decode(out: Input): Either[ErrorTree,B] = orig.decode(out) >>= f
      def render(
        pageKey: List[String],
        fieldKey: List[String],        
        breadcrumbs: Breadcrumbs,
        data: Input,
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Html = orig.render(pageKey, fieldKey, breadcrumbs, data, errors, messages)
    }
  }

  def getPage(
    key: List[String],
    state: DB,
    existing: Input,
    breadcrumbs: Breadcrumbs,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Html =
    render(key, key.last :: Nil, breadcrumbs, existing, ErrorTree.empty, messages)

  def postPage(
    key: List[String],
    state: DB,
    request: Input,
    errors: ErrorTree,
    breadcrumbs: Breadcrumbs,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Html =
    render(key, key.last :: Nil, breadcrumbs, request, errors, messages)

    def codec: Codec[A] = this
}
