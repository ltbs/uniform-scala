package ltbs.uniform
package common.web

import magnolia._
import scala.language.experimental.macros
import cats.implicits._

trait InferCodec {

  type Typeclass[T] = Codec[T]

  def combine[T](caseClass: CaseClass[Typeclass, T]) = new Codec[T] {
    def decode(out: Input): Either[ErrorTree,T] = {
      caseClass.constructEither {
        p => p.typeclass.decode(out / p.label).leftMap{_.prefixWith(p.label)}
      }.leftMap{_.combineAll}
    }

    def encode(in: T): Input = {
      val members = caseClass.parameters.map { p =>
        p.typeclass.encode(p.dereference(in)).prefixWith(p.label)
      }
      members.toList.combineAll
    }
  }
  
  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = new Codec[T] {
    def decode(out: Input): Either[ErrorTree,T] = {
      sealedTrait.subtypes.collectFirst{
        case subtype if List(subtype.typeName.short).some === out.valueAtRoot =>
          subtype.typeclass.decode(out / subtype.typeName.short).leftMap{_.prefixWith(subtype.typeName.short)}
      }.getOrElse(ErrorMsg("required").toTree.asLeft)
    }

    def encode(in: T): Input = {
      sealedTrait.dispatch(in) { subtype =>
        subtype.typeclass.encode(subtype.cast(in)).prefixWith(subtype.typeName.short) |+|
        (Map(Nil -> List(subtype.typeName.short)): Input)
      } 
    }
  }

  implicit def gen[T]: Codec[T] = macro Magnolia.gen[T]
}

object InferCodec extends InferCodec {
  def apply[T](implicit codec: Codec[T]): Codec[T] = codec
}
