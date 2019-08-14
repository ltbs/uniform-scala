package ltbs.uniform
package common.web

import shapeless._, labelled._
import cats.Monoid
import com.github.ghik.silencer.silent

trait InferFormFieldPresentation[Html] {

  type FF[A] = FormFieldPresentation[A, Html]

  implicit def hnilField(implicit mon: Monoid[Html]) = new FF[HNil] {
    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = Monoid[Html].empty
  }

  implicit def hConsField[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hField: Lazy[FF[H]],
    tField: FF[T],
    mon: Monoid[Html]
  ): FF[FieldType[K,H] :: T] = new FF[FieldType[K,H] :: T] {
    val fieldName: String = witness.value.name

    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = Monoid[Html].combine(
      hField.value.render(key :+ fieldName, path, data / fieldName, errors / fieldName, messages),
      tField.render(key, path, data, errors, messages)
    )
  }

  implicit def genericField[A, H, T](implicit
    @silent generic: LabelledGeneric.Aux[A,T],
    hGenParser: Lazy[FF[T]]
  ): FF[A] = hGenParser.value.mapToType

  /** We get this from coproduct anyway, but want `Some` to appear
    * above `None`. In the future once there is a way to easily
    * override the order this (and
    * [[InferFormFieldEncoding.optionParser]]) will no longer be
    * needed.
    */
  implicit def optionField[A](
    implicit ffSome: FF[A],
    mon: Monoid[Html]
  ) = new FF[Option[A]] {
    def render(
      key: List[String],
      path: Path,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      val options: List[(String, (List[String], Path, Input, ErrorTree, UniformMessages[Html]) => Html)] =
        List(
          "Some" -> {case (subKey, subPath, subOpt, subErr, subMsg) =>
            ffSome.render(
              subKey :+ "value",
              subPath,
              subOpt / "value",
              subErr / "value",
              subMsg
            )},
          "None" -> {case _ => Monoid[Html].empty}
        )

      selectionOfFields(options)(key,path, data,errors,messages)
    }
  }


  // COPRODUCTS
  def selectionOfFields(
    inner: List[(String, (List[String], Path, Input, ErrorTree, UniformMessages[Html]) => Html)]
  )(
    key: List[String],
    path: Path,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  case class CoproductFieldList[A](
    inner: List[(String, (List[String], Path, Input, ErrorTree, UniformMessages[Html]) => Html)]
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
      def render(key: List[String], path: Path, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html =
        selectionOfFields(coproductFields.inner)(key,path, values,errors,messages)
    }
}
