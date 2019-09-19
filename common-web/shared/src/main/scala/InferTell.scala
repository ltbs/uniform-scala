package ltbs.uniform
package common.web

import language.implicitConversions

import shapeless._, labelled._, tag._
import collection.immutable.ListMap

trait InferTell[Html] {
  trait FieldMapTag

  type FieldMap = ListMap[List[String], Html] @@ FieldMapTag

  type FieldFunction[A] = (A, UniformMessages[Html]) => FieldMap
  protected implicit def toFieldMap(m: ListMap[List[String], Html]): FieldMap =
    tag[FieldMapTag][ListMap[List[String], Html]](m)

  implicit def hnilTellField: FieldFunction[HNil] =
    (_,_) => ListMap.empty[List[String], Html]

  implicit def hConsTellField[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hField: Lazy[FieldFunction[H]],
    tField: FieldFunction[T]
  ): FieldFunction[FieldType[K,H] :: T] = (x,messages) => hField.value(x.head, messages).map{
    case (k, v) => (witness.value.name :: k, v)
  } ++ tField(x.tail, messages)

  def mapToHtml(m: ListMap[List[String], Html], messages: UniformMessages[Html]): Html

  implicit def genericTellField[A, H, T](implicit
    generic: LabelledGeneric.Aux[A,T],
    hlistInstance: Lazy[FieldFunction[T]]
  ): FieldFunction[A] = (x, m) => hlistInstance.value.apply(generic.to(x), m)

  implicit def fieldFunctionToGenericWebTell[A](implicit
    ff: FieldFunction[A]
  ): GenericWebTell[A, Html] = new GenericWebTell[A, Html] {
    def render(in: A, key: String, messages: UniformMessages[Html]) =
      mapToHtml(ff(in, messages), messages)
  }  
}
