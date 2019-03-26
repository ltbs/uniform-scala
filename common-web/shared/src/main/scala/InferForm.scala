package ltbs.uniform.web

import play.twirl.api.Html
import shapeless._
import shapeless.labelled._
import cats.implicits._
import cats.Monoid
import ltbs.uniform._

trait HtmlField[A]{
  def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html
}

trait HtmlForm[A]{
  def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html], tell: Html = Html("")): Html
}

trait InferForm {

  def errorSummary(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html  
  def soloField(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html])(inner: Html)(tell: Html): Html
  def compoundField(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html])(inner: Html): Html
  def selectionOfFields(
    inner: List[(String, (String, Input, ErrorTree, UniformMessages[Html]) => Html)]
  )(
    key: String,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html]
  ): Html

  implicit val hnilField = new HtmlField[HNil] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html =
      Monoid[Html].empty
  }

  implicit def hConsField[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hField: Lazy[HtmlField[H]],
    tField: HtmlField[T]
  ): HtmlField[FieldType[K,H] :: T] = new HtmlField[FieldType[K,H] :: T] {
    val fieldName: String = witness.value.name

    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html =
      compoundField(s"${key}.${fieldName}", values, errors, messages)(
      hField.value.render(s"${key}.${fieldName}", values, errors, messages)) |+|
        tField.render(key, values, errors, messages)
  }

  implicit def genericField[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
     hGenParser: Lazy[HtmlField[T]]
    ): HtmlField[A] = new HtmlField[A]
  {

    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html =
      hGenParser.value.render(key, values, errors, messages)
  }

  // COPRODUCTS

  case class CoproductFieldList[A](
    inner: List[(String, (String, Input, ErrorTree, UniformMessages[Html]) => Html)]
  )
  implicit val cnilField: CoproductFieldList[CNil] = CoproductFieldList(List.empty)

  implicit def coproductFieldList[K <: Symbol, H, T <: Coproduct](
    implicit
      witness: Witness.Aux[K],
    hField: HtmlField[H],
    tFields: CoproductFieldList[T]
  ): CoproductFieldList[FieldType[K, H] :+: T] = CoproductFieldList (
    (witness.value.name, hField.render _) :: tFields.inner
  )

  implicit def coproductField[A](implicit coproductFields: CoproductFieldList[A]) =
    new HtmlField[A] {
      def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html =
        selectionOfFields(coproductFields.inner)(key,values,errors,messages)
    }

  implicit def simpleForm[A](implicit field: HtmlField[A]): HtmlForm[A] = new HtmlForm[A] {
    def render(key: String, values: Input, errors: ErrorTree, messages: UniformMessages[Html], tell: Html): Html =
      errorSummary(key, values, errors, messages) |+|
      soloField(key, values, errors, messages)(field.render(key, values, errors, messages))(tell)
  }

}
