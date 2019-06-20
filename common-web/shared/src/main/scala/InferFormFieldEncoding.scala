package ltbs.uniform
package common.web

import shapeless._, labelled._
import cats.implicits.{catsSyntaxEither => _,_}
import com.github.ghik.silencer.silent

object InferFormFieldEncoding extends InferFormFieldEncoding
trait InferFormFieldEncoding {

  implicit val hnilParser = new FormFieldEncoding[HNil] {
    def decode(out: Input): Either[ErrorTree,HNil] = Right(HNil)
    def encode(in: HNil): Input = Input.empty
  }

  implicit val cnilParser: FormFieldEncoding[CNil] =
    new FormFieldEncoding[CNil] {
      def decode(in: Input): Either[ErrorTree,CNil] =
        Left(ErrorMsg("required").toTree)
      def encode(a: CNil): Input = Input.empty
    }

  implicit def hConsParser[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hParser: Lazy[FormFieldEncoding[H]],
    tParser: FormFieldEncoding[T]
  ): FormFieldEncoding[FieldType[K,H] :: T] = new FormFieldEncoding[FieldType[K,H] :: T] {
    val fieldName: String = witness.value.name
    def decode(out: Input): Either[ErrorTree,FieldType[K,H] :: T] = {
      (
        hParser.value.decode(out / fieldName),
        tParser.decode(out)
      ) match {
        case (Right(h), Right(t)) => Right((field[K](h) :: t))
        case (Left(he), Left(te)) => Left(he.prefixWith(fieldName) |+| te)
        case (_,        Left(te)) => Left(te)
        case (Left(he), _)        => Left(he.prefixWith(fieldName))
      }
    }

    def encode(a: FieldType[K,H] :: T): Input = {
      val tailData: Input = tParser.encode(a.tail)
      val headData: Input = hParser.value.encode(a.head)
      tailData |+| headData.prefixWith(fieldName)
    }
  }

  implicit def coproductParser[K <: Symbol, H, T <: Coproduct](
    implicit
      witness: Witness.Aux[K],
    hparser: FormFieldEncoding[H],
    tparser: FormFieldEncoding[T]
  ): FormFieldEncoding[FieldType[K, H] :+: T] =
    new FormFieldEncoding[FieldType[K, H] :+: T] {

      val fname = witness.value.name

      def decode(in: Input): Either[ErrorTree,FieldType[K, H] :+: T] = {
        if (in.valueAtRoot.headOption == Some(List(fname))) {
          hparser.decode(
            in / fname
          ).map{x => Inl(field[K]{x})} match {
            case Left(e) => Left(e.prefixWith(fname))
            case r@Right(_) => r
          }
        } else {
          tparser.decode(in).map(x => Inr(x))
        }
      }

      def encode(a: FieldType[K, H] :+: T): Input = a match {
        case Inl(l) =>
          hparser.encode(l).prefixWith(fname) ++ Map(Nil -> List(fname))
        case Inr(r) => tparser.encode(r)
      }
    }

  implicit def genericParser[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
      hGenParser: Lazy[FormFieldEncoding[T]],
      @silent lp: LowPriority
    ): FormFieldEncoding[A] = new FormFieldEncoding[A]
  {
    def decode(in: Input): Either[ErrorTree,A] =
      hGenParser.value.decode(in).map(generic.from)

    def encode(a:A): Input =
      hGenParser.value.encode(generic.to(a))
  }
}
