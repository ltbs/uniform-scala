package ltbs.uniform
package common.web

import cats.syntax.either._
import cats.syntax.flatMap._
import scala.concurrent.ExecutionContext
import com.github.ghik.silencer.silent
import validation._

/** Defines both the rendering and the encoding for a given datatype */
trait WebAsk[Html, A] extends Codec[A] {

  def render(
    pageIn: PageIn[Html],
    stepDetails: StepDetails[Html, A],
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
        pageIn: PageIn[Html],
        stepDetails: StepDetails[Html, B]
      ): Option[Html] = {
        orig.render(pageIn, stepDetails.copy(validation = Rule.alwaysPass))
      }
    }
  }

  def codec: Codec[A] = this
}
