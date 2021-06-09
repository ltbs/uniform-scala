package ltbs.uniform
package common.web

import magnolia._
import scala.language.experimental.macros
import cats.implicits._

trait InferFormFields[Html] {

  @annotation.implicitAmbiguous("Unable to ask[Traversable[${A}]] - consider using askList[${A}] instead")
  implicit def blockCollections[X[_] <: Traversable[_], A]: FormField[Html, X[A]] = ???
  implicit def blockCollections2[X[_] <: Traversable[_], A]: FormField[Html, X[A]] = ???  

  def renderAnd(
    pageKey: List[String],
    fieldKey: List[String],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    members: Seq[(String, Html)]
  ): Html

  def renderOr(
    pageKey: List[String],
    fieldKey: List[String],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    alternatives: Seq[(String, Option[Html])],
    selected: Option[String]
  ): Html

  type Typeclass[T] = FormField[Html, T] // is this needed?
  def combine[T](caseClass: CaseClass[FormField[Html, ?], T]) = new FormField[Html, T] {
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

    // Members declared in ltbs.uniform.common.web.FormField
    def render(
      pageKey: List[String],
      fieldKey: List[String],
      breadcrumbs: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Option[Html] =

    if (caseClass.isObject) None else 
    renderAnd(
      pageKey,
      fieldKey,
      breadcrumbs,
      data,
      errors,
      messages,
      caseClass.parameters.map {
        p => p.label -> p.typeclass.render(
          pageKey,
          fieldKey :+ p.label,
          breadcrumbs,
          data / p.label,
          errors / p.label,
          messages
        )
      }.collect{ case (k, Some(v)) => (k,v)}
    ).some
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = new FormField[Html, T] {
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

    // Members declared in ltbs.uniform.common.web.FormField
    def render(
      pageKey: List[String],
      fieldKey: List[String],
      breadcrumbs: Breadcrumbs,
      data: Input,
      errors: ErrorTree,
      messages: UniformMessages[Html]
    ): Option[Html] = {
      renderOr(
        pageKey,
        fieldKey,
        breadcrumbs,
        data,
        errors,
        messages,
        sealedTrait.subtypes.map{ subtype =>
          (
            subtype.typeName.short,
            {
            subtype.typeclass.render(
              pageKey,
              fieldKey :+ subtype.typeName.short,
              breadcrumbs,
              data / subtype.typeName.short,
              errors / subtype.typeName.short,
              messages
            )
            }
          )
        },
        sealedTrait.subtypes.collectFirst {
          case subtype if List(subtype.typeName.short).some === data.valueAtRoot =>
            subtype.typeName.short
        }
      )
    }.some
  }

  implicit def gen[T]: FormField[Html, T] = macro Magnolia.gen[T]
}
