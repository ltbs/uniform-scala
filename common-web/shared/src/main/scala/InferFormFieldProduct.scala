package ltbs.uniform
package common.web

import shapeless._, labelled._
import com.github.ghik.silencer.silent
import cats.implicits.{catsSyntaxEither => _,_}

trait ProductFieldList[A, Html]{
  def decode(out: Input): Either[ErrorTree,A]
  def encode(in: A): Input
  def stats: FormFieldStats
  val inner: List[(String, (List[String], List[String], Breadcrumbs, Input, ErrorTree, UniformMessages[Html]) => Html)]
}

trait InferFormFieldProduct[Html] {

  def renderProduct[A](
    pageKey: List[String],
    fieldKey: List[String],    
    path: Breadcrumbs,
    values: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    pfl: ProductFieldList[A, Html]
  ): Html

  implicit val hnilFieldList: ProductFieldList[HNil, Html] = new ProductFieldList[HNil, Html]{
    override def decode(in: Input): Either[ErrorTree,HNil] = Right(HNil)
    override def encode(a: HNil): Input = Input.empty
    override val inner = List.empty
    def stats = FormFieldStats()    
  }

  implicit def consFieldList[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hField: Lazy[FormField[H, Html]],
    tFields: ProductFieldList[T, Html]
  ): ProductFieldList[FieldType[K, H] :: T, Html] =
    new ProductFieldList[FieldType[K, H] :: T, Html] {
      val fieldName = witness.value.name
      val inner = (fieldName, hField.value.render _) :: tFields.inner

      def decode(out: Input): Either[ErrorTree,FieldType[K, H] :: T] = {
        (
          hField.value.decode(out / fieldName),
          tFields.decode(out)
        ) match {
          case (Right(h), Right(t)) => Right((field[K](h) :: t))
          case (Left(he), Left(te)) =>
            val l = he.prefixWith(fieldName)
          Left(l ++ te)
          case (_,        Left(te)) => Left(te)
          case (Left(he), _)        => Left(he.prefixWith(fieldName))
        }
      }

      def encode(a: FieldType[K, H] :: T): Input =  {
        val tailData: Input = tFields.encode(a.tail)
        val headData: Input = hField.value.encode(a.head)
        tailData |+| headData.prefixWith(fieldName)
      }

      override def stats = FormFieldStats(
        children = tFields.stats.children + 1,
        compoundChildren =
          tFields.stats.compoundChildren + {if (hField.value.stats.isCompound) 1 else 0}
      )
    }

  implicit def productField[A](implicit productFields: ProductFieldList[A,Html]) =
    new FormField[A, Html] {
      def render(
        pageKey: List[String],
        fieldKey: List[String],        
        path: Breadcrumbs,
        values: Input,
        errors: ErrorTree,
        messages: UniformMessages[Html]
      ): Html =
        renderProduct(pageKey, fieldKey, path, values, errors, messages, productFields)

      def decode(out: Input): Either[ErrorTree,A] = productFields.decode(out)
      def encode(in: A): Input = productFields.encode(in)

      override def stats = productFields.stats
    }

  implicit def genericField[A, H, T](implicit
    @silent("never used") generic: LabelledGeneric.Aux[A,T],
    hlistInstance: Lazy[FormField[T, Html]]
  ): FormField[A, Html] = new FormField[A, Html] {

    val hlist = hlistInstance.value
    def decode(in: Input): Either[ErrorTree,A] =
      hlist.decode(in).map(generic.from)

    def encode(a: A): Input =
      hlist.encode(generic.to(a))

    def render(
      pageKey: List[String],
      fieldKey: List[String],      
      path: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Html = {
      hlist.render(pageKey, fieldKey, path, data, errors, messages)
    }

    override def stats = hlist.stats
  }

}
