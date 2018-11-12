package ltbs.uniform.html

import scalatags._
import shapeless._
import shapeless.labelled._

class SharedTemplates[Builder, Output <: FragT, FragT](
  val bundle: generic.Bundle[Builder, Output, FragT]
) {
  import bundle.all._
  val widget: Tag = div("hello")

  type Errors = Map[String, String]

  trait FormControl[A]{
    def apply(key: String, value: Option[A], errors: Errors): Tag
  }

  def formControl[A](f: (String, Option[A], Map[String,String]) => Tag): FormControl[A] = new FormControl[A] {
    def apply(key: String, value: Option[A], errors: Errors): Tag = f(key, value, errors)
  }

  def formControl[A](f: (String, Option[A]) => Tag): FormControl[A] = new FormControl[A] {
    def apply(key: String, value: Option[A], errors: Errors): Tag = f(key, value)
  }

  implicit def optControl[A](implicit inner: FormControl[A]): FormControl[Option[A]] = formControl {
    (key: String, outerValue: Option[Option[A]], errors: Errors) =>
      fieldset(
        legend(key),
        input(`type`:="radio", name:=key ++ "_outer", value:="yes", id:=key ++ "_outer_yes"),
        label(`for`:=key ++ "_outer_yes", "yes"),
        input(`type`:="radio", name:=key ++ "_outer", value:="no", id:=key ++ "_outer_no"),
        label(`for`:=key ++ "_outer_no", "no"),
        inner(key ++ "_inner", outerValue.flatten, errors)
      )
  }

  implicit val intControl: FormControl[Int] = formControl {
    (key: String, existing: Option[Int], errors: Errors) =>
    input(name:=key, value:=existing.fold("")(_.toString), id:=key)
  }

  implicit val stringControl: FormControl[String] = formControl {
    (key: String, existing: Option[String], errors: Errors) =>
    input(name:=key, value:=existing.getOrElse(""), id:=key)
  }

  implicit val booleanControl: FormControl[Boolean] = formControl {
    (key: String, existing: Option[Boolean], errors: Errors) =>
    fieldset(
      input(`type`:="radio", name:=key, value:="yes", id:=key ++ "_yes"),
      label(`for`:=key ++ "_yes", "yes"),
      input(`type`:="radio", name:=key, value:="no", id:=key ++ "_no"),
      label(`for`:=key ++ "_no", "no"),
    )
  }

  implicit val hnilControl: FormControl[HNil] = formControl {
    (key: String, opt: Option[HNil]) => div{} }

  implicit def hConsControl[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hControl: Lazy[FormControl[H]],
    tControl: FormControl[T]
  ): FormControl[FieldType[K,H] :: T] = formControl {
    (key: String, v: Option[FieldType[K,H] :: T], errors: Errors) => {
      val fieldName: String = witness.value.name
      val head = hControl.value.apply(fieldName, v.map{_.head}, errors)
      val tail = tControl.apply(key, v.map{_.tail}, errors)
      div(head, tail)
    }
  }

  implicit def genericControl[A, H, T]
    (implicit
       generic: LabelledGeneric.Aux[A,T],
     hGenControl: Lazy[FormControl[T]]
    ): FormControl[A] = formControl { (key: String, v: Option[A], err: Errors) =>
    hGenControl.value.apply(key, v.map{generic.to}, err)
  }

}
