package ltbs.uniform
package common.web
import cats.implicits._

trait Codec[A]{
  def encode(in: A): Input
  def decode(out: Input): Either[ErrorTree,A]

  def imap[B](f: A => B)(g: B => A): Codec[B] =
    simap[B](f(_).asRight)(g)

  def simap[B](f: A => Either[ErrorTree,B])(g: B => A): Codec[B] = {
    val orig = this

    new Codec[B] {
      def encode(in: B): Input = orig.encode(g(in))
      def decode(out: Input): Either[ErrorTree,B] = orig.decode(out) >>= f
    }
  }

}
