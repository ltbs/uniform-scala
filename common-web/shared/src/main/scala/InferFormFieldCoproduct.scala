package ltbs.uniform
package common.web

import shapeless._, labelled._

trait CoproductFieldList[A, Html]{
  def decode(out: Input): Either[ErrorTree,A]
  def encode(in: A): Input
  val inner: List[(String, (List[String], List[String], Breadcrumbs, Input, ErrorTree, UniformMessages[Html]) => Html)]
}

trait InferFormFieldCoProduct[Html] {

  def renderCoproduct[A](
    pageKey: List[String],    
    fieldKey: List[String],
    path: Breadcrumbs,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    cfl: CoproductFieldList[A,Html]
  ): Html

  implicit val cnilField: CoproductFieldList[CNil,Html] = new CoproductFieldList[CNil,Html]{

    override def decode(in: Input): Either[ErrorTree,CNil] =
      Left(ErrorMsg("required").toTree)
    override def encode(a: CNil): Input = Input.empty
    override val inner = List.empty
  }

  implicit def coproductFieldList[K <: Symbol, H, T <: Coproduct](
    implicit
      witness: Witness.Aux[K],
    hField: FormField[H, Html],
    tFields: CoproductFieldList[T, Html]
  ): CoproductFieldList[FieldType[K, H] :+: T, Html] =
    new CoproductFieldList[FieldType[K, H] :+: T, Html] {
    val fname = witness.value.name
    val inner = (fname, hField.render _) :: tFields.inner

    def decode(in: Input): Either[ErrorTree,FieldType[K, H] :+: T] = {
      if (in.valueAtRoot.headOption == Some(List(fname))) {
        hField.decode(
          in / fname
        ).map{x => Inl(field[K]{x})} match {
          case Left(e) => Left(e.prefixWith(fname))
          case r@Right(_) => r
        }
      } else {
        tFields.decode(in).map(x => Inr(x))
      }
    }

    def encode(a: FieldType[K, H] :+: T): Input = a match {
      case Inl(l) =>
        hField.encode(l).prefixWith(fname) ++ Map(Nil -> List(fname))
      case Inr(r) => tFields.encode(r)
    }
  }

  implicit def coproductField[A](implicit coproductFields: CoproductFieldList[A, Html]) =
    new FormField[A, Html] {
      def render(pageKey: List[String], fieldKey: List[String], path: Breadcrumbs, values: Input, errors: ErrorTree, messages: UniformMessages[Html]): Html =
        renderCoproduct(pageKey, fieldKey, path, values, errors, messages, coproductFields)

      def decode(out: Input): Either[ErrorTree,A] = coproductFields.decode(out)
      def encode(in: A): Input = coproductFields.encode(in)
    }

}
