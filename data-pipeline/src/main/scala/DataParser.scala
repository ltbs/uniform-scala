package ltbs.uniform.datapipeline

import simulacrum._
import cats.implicits._

@typeclass
trait DataParser[A] {
  def bind(in: Input): Either[Error,A]
  def unbind(a:A): Input

  def transform[B](f: A => Either[Error,B], g: B => A): DataParser[B] = {
    val parent = this
    new DataParser[B] {
      def bind(in: Input): Either[Error,B] = parent.bind(in).flatMap(f)
      def unbind(in: B): Input = parent.unbind(g(in))
    }
  }

  def validating(f: PartialFunction[A,Error]): DataParser[A] = {
    def optToEither(in: A): Either[Error,A] = f.lift(in).fold(in.asRight[Error])(_.asLeft[A])
    transform(optToEither, identity)
  }
}
