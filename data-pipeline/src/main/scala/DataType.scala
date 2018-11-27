package ltbs.uniform.datapipeline

import cats.implicits._
import cats.data.Validated
import cats.Invariant

object DataType {

  implicit val dataTypeInvariant = new Invariant[DataType] {
    def imap[A, B](fa: DataType[A])(f: A => B)(g: B => A): DataType[B] = new DataType[B] {
      def from(in: FormUrlEncoded): Validated[Error, B] = fa.from(in).map(f)
      def to(out: B): FormUrlEncoded = fa.to(g(out))
    }
  }


  trait DataType[A] {
    def from(in: FormUrlEncoded): Validated[Error, A]
    def to(out: A): FormUrlEncoded
  }

}
