package ltbs.uniform
package common.web

import cats.syntax.either._
import cats.syntax.flatMap._
import scala.concurrent.ExecutionContext
import com.github.ghik.silencer.silent

/** Defines both the rendering and the encoding for a given datatype */
trait WebAsk[Html, A] extends Codec[A] {

  def render(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Html],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Option[Html]

  /** Produce a new `WebAsk` from this one by mapping the types */
  override def imap[B](f: A => B)(g: B => A): WebAsk[Html, B] =
    simap[B](f(_).asRight)(g)

  /** Produce a new `WebAsk` from this one, with the
    * possibility of extending the validation
    */
  override def simap[B](f: A => Either[ErrorTree,B])(g: B => A): WebAsk[Html, B] = {
    val orig = this

    new WebAsk[Html, B] {
      def encode(in: B): Input = orig.encode(g(in))
      def decode(out: Input): Either[ErrorTree,B] = orig.decode(out) >>= f
      def render(
        pageKey: List[String],
        fieldKey: List[String],
        tell: Option[Html],
        breadcrumbs: Breadcrumbs,
        data: Input,
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Option[Html] = orig.render(pageKey, fieldKey, tell, breadcrumbs, data, errors, messages)
    }
  }

  @silent("never used")
  def getPage(
    key: List[String],
    tell: Option[Html],    
    state: DB,
    existing: Input,
    breadcrumbs: Breadcrumbs,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Option[Html] =
    render(key, key.last :: Nil, tell, breadcrumbs, existing, ErrorTree.empty, messages)

  @silent("never used")  
  def postPage(
    key: List[String],
    tell: Option[Html],        
    state: DB,
    request: Input,
    errors: ErrorTree,
    breadcrumbs: Breadcrumbs,
    messages: UniformMessages[Html]
  )(implicit ec: ExecutionContext): Option[Html] =
    render(key, key.last :: Nil, tell, breadcrumbs, request, errors, messages)
    def codec: Codec[A] = this
}
