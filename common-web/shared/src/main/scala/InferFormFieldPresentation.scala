package ltbs.uniform
package common.web

import shapeless._, labelled._
import cats.Monoid
import com.github.ghik.silencer.silent

abstract class InferFormFieldPresentation[Html: Monoid] {

  type FF[A] = FormFieldPresentation[A, Html]

  implicit val hnilField = new FF[HNil] {
    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = Monoid[Html].empty
  }

  implicit def hConsField[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hField: Lazy[FF[H]],
    tField: FF[T]
  ): FF[FieldType[K,H] :: T] = new FF[FieldType[K,H] :: T] {
    val fieldName: String = witness.value.name

    def render(
      key: List[String],
      path: Path,
      data: Option[Input],
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = Monoid[Html].combine(
      hField.value.render(key :+ fieldName, path, data.map{_ / fieldName}, errors / fieldName, messages),
      tField.render(key, path, data, errors, messages)
    )
  }
  
  implicit def genericField[A, H, T](implicit
    @silent generic: LabelledGeneric.Aux[A,T],
    hGenParser: Lazy[FF[T]],
    @silent lp: LowPriority
  ): FF[A] = hGenParser.value.mapToType

  // COPRODUCTS
  def selectionOfFields(
    inner: List[(String, (List[String], Path, Option[Input], ErrorTree, UniformMessages[Html]) => Html)]
  )(
    key: List[String],
    path: Path, 
    values: Option[Input],
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  case class CoproductFieldList[A](
    inner: List[(String, (List[String], Path, Option[Input], ErrorTree, UniformMessages[Html]) => Html)]
  )

  implicit val cnilField: CoproductFieldList[CNil] = CoproductFieldList(List.empty)

  implicit def coproductFieldList[K <: Symbol, H, T <: Coproduct](
    implicit
      witness: Witness.Aux[K],
    hField: FF[H],
    tFields: CoproductFieldList[T]
  ): CoproductFieldList[FieldType[K, H] :+: T] = CoproductFieldList (
    (witness.value.name, hField.render _) :: tFields.inner
  )

  implicit def coproductField[A](implicit coproductFields: CoproductFieldList[A]) =
    new FormFieldPresentation[A, Html] {
      def render(key: List[String], path: Path, values: Option[Input], errors: ErrorTree, messages: UniformMessages[Html]): Html =
        selectionOfFields(coproductFields.inner)(key,path, values,errors,messages)
    }
}
