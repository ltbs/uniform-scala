package ltbs.uniform.web

import simulacrum._
import cats.implicits._
import scala.language.implicitConversions
import ltbs.uniform.ErrorTree

@typeclass
trait DataParser[A] {
  def bind(in: Input): Either[ErrorTree,A]
  def unbind(a:A): Input

  def transform[B](f: A => Either[ErrorTree,B], g: B => A): DataParser[B] = {
    val parent = this
    new DataParser[B] {
      def bind(in: Input): Either[ErrorTree,B] = parent.bind(in).flatMap(f)
      def unbind(in: B): Input = parent.unbind(g(in))
    }
  }

  def validating(f: PartialFunction[A,ErrorTree]): DataParser[A] = {
    def optToEither(in: A): Either[ErrorTree,A] = f.lift(in).fold(in.asRight[ErrorTree])(_.asLeft[A])
    transform(optToEither, identity)
  }
}
