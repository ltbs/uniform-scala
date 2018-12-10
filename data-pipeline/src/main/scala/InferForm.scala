package ltbs.uniform.datapipeline

import play.twirl.api.Html
import shapeless._
import shapeless.labelled._
import cats.implicits._
import cats.Monoid

trait HtmlField[A]{
  def render(key: String, values: Input, errors: Error, messages: Messages): Html
}

trait HtmlForm[A]{
  def render(key: String, values: Input, errors: Error, messages: Messages): Html
}

trait InferForm {

  def errorSummary(key: String, values: Input, errors: Error, messages: Messages): Html  
  def soloField(key: String, values: Input, errors: Error, messages: Messages)(inner: Html): Html
  def compoundField(key: String, values: Input, errors: Error, messages: Messages)(inner: Html): Html

  implicit val htmlMonoidInstance = new Monoid[Html] {
    def empty: Html = Html("")
    def combine(a: Html, b: Html):Html = Html(a.toString ++ b.toString)
  }

  implicit val hnilField = new HtmlField[HNil] {
    def render(key: String, values: Input, errors: Error, messages: Messages): Html =
      Monoid[Html].empty
  }

  implicit def hConsField[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hField: Lazy[HtmlField[H]],
    tField: HtmlField[T]
  ): HtmlField[FieldType[K,H] :: T] = new HtmlField[FieldType[K,H] :: T] {
    val fieldName: String = witness.value.name

    def render(key: String, values: Input, errors: Error, messages: Messages): Html =
      compoundField(s"${key}.${fieldName}", values, errors, messages)(
      hField.value.render(s"${key}.${fieldName}", values, errors, messages)) |+|
        tField.render(key, values, errors, messages)
  }

  def genericField[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
     hGenParser: Lazy[HtmlField[T]]
    ): HtmlField[A] = new HtmlField[A]
  {

    def render(key: String, values: Input, errors: Error, messages: Messages): Html =
      hGenParser.value.render(key, values, errors, messages)
  }

  implicit def simpleForm[A](implicit field: HtmlField[A]): HtmlForm[A] = new HtmlForm[A] {
    def render(key: String, values: Input, errors: Error, messages: Messages): Html =
      errorSummary(key, values, errors, messages) |+|
      soloField(key, values, errors, messages)(field.render(key, values, errors, messages))
  }

  implicit def compoundForm[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
     hGenParser: Lazy[HtmlField[T]]
    ): HtmlForm[A] =
    new HtmlForm[A] {
      def render(key: String, values: Input, errors: Error, messages: Messages): Html =
      errorSummary(key, values, errors, messages) |+|
        soloField(key, values, Tree(errors.value), messages)(
          genericField(generic, hGenParser).render(key, values, errors, messages)
        )
    }

}
