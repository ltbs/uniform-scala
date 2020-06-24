package ltbs.uniform
package common.web

import magnolia._
import scala.language.experimental.macros
import cats.implicits._

trait InferFormFields[Html] {

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
    alternatives: Seq[(String, Html)],
    selected: Option[String]
  ): Html

  type Typeclass[T] = FormField[T, Html] // is this needed?
  def combine[T](caseClass: CaseClass[FormField[?, Html], T]) = new FormField[T, Html] {
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
    ): Html = renderAnd(
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
      }
    )
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = new FormField[T, Html] {
    def decode(out: Input): Either[ErrorTree,T] = {
      sealedTrait.subtypes.collectFirst{
        case subtype if List(subtype.typeName.full).some === out.valueAtRoot =>
          subtype.typeclass.decode(out / subtype.typeName.full)
      }.getOrElse(ErrorMsg("required").toTree.asLeft)
    }

    def encode(in: T): Input = {
      sealedTrait.dispatch(in) { subtype =>
        subtype.typeclass.encode(subtype.cast(in)).prefixWith(subtype.typeName.full) |+|
        (Map(Nil -> List(subtype.typeName.full)): Input)
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
    ): Html = {
      renderOr(
        pageKey,
        fieldKey,
        breadcrumbs,
        data,
        errors,
        messages,
        sealedTrait.subtypes.map{ subtype => 
          (
            subtype.typeName.full,
            subtype.typeclass.render(
              pageKey,
              fieldKey :+ subtype.typeName.full,
              breadcrumbs,
              data / subtype.typeName.full,
              errors / subtype.typeName.full,
              messages
            )
          )
        },
        sealedTrait.subtypes.collectFirst {
          case subtype if List(subtype.typeName.full).some === data.valueAtRoot =>
            subtype.typeName.full
        }
      )
    }
  }

  implicit def gen[T]: FormField[T, Html] = macro Magnolia.gen[T]
}
