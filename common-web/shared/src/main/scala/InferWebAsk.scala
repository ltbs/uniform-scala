package ltbs.uniform
package common.web

import magnolia._
import scala.language.experimental.macros
import cats.implicits._
import scala.language.higherKinds

trait InferWebAsk[Html] {

  @annotation.implicitAmbiguous("Unable to ask[Traversable[${A}]] - consider using askList[${A}] instead")
  implicit def blockCollections[X[_] <: Traversable[_], A]: WebAsk[Html, X[A]] = ???
  implicit def blockCollections2[X[_] <: Traversable[_], A]: WebAsk[Html, X[A]] = ???  

  def renderAnd(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Html],
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    members: Seq[(String, Html)]
  ): Html

  def renderOr(
    pageKey: List[String],
    fieldKey: List[String],
    tell: Option[Html],    
    breadcrumbs: Breadcrumbs,
    data: Input,
    errors: ErrorTree,
    messages: UniformMessages[Html],
    alternatives: Seq[(String, Option[Html])],
    selected: Option[String]
  ): Html

  type Typeclass[T] = WebAsk[Html, T]

  def combine[T](caseClass: CaseClass[Typeclass, T]) = new WebAsk[Html, T] {
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

    def render(
      pageIn: PageIn[Html],
      pageKey: List[String],
      fieldKey: List[String],
      tell: Option[Html], 
      data: Input,
      errors: ErrorTree
    ): Option[Html] =

    if (caseClass.isObject) None else 
    renderAnd(
      pageKey,
      fieldKey,
      tell, 
      pageIn.breadcrumbs,
      data,
      errors,
      pageIn.messages,
      caseClass.parameters.map {
        p => p.label -> p.typeclass.render(
          pageIn,
          pageKey,
          fieldKey :+ p.label,
          tell, 
          data / p.label,
          errors / p.label
        )
      }.collect{ case (k, Some(v)) => (k,v)}
    ).some
  }
  
  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T]): Typeclass[T] = new WebAsk[Html, T] {
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

    def render(
      pageIn: PageIn[Html],
      pageKey: List[String],
      fieldKey: List[String],
      tell: Option[Html], 
      data: Input,
      errors: ErrorTree
    ): Option[Html] = {
      renderOr(
        pageKey,
        fieldKey,
        tell,
        pageIn.breadcrumbs,
        data,
        errors,
        pageIn.messages,
        sealedTrait.subtypes.map{ subtype =>
          (
            subtype.typeName.short,
            {
              subtype.typeclass.render(
                pageIn, 
                pageKey,
                fieldKey :+ subtype.typeName.short,
                tell,
                data / subtype.typeName.short,
                errors / subtype.typeName.short
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

  implicit def gen[T]: WebAsk[Html, T] = macro Magnolia.gen[T]
}
