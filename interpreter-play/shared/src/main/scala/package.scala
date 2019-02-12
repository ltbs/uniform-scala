package ltbs.uniform.interpreters

import cats.data._
import cats.Invariant
import play.api.data.Form
import play.api.mvc.{ Request, AnyContent }
import play.twirl.api.Html
import ltbs.uniform._

package object playframework {

  type PlayForm[OUT] = SimpleInteractionForm[Request[AnyContent],OUT,Html]

  type Encoded = String

  type ValidationError = String
  type ValidatedData[A] = Option[Validated[ValidationError, A]]

  implicit val playFormFunctor: Invariant[Form] = new Invariant[Form]{
    def imap[A, B](fa: Form[A])(f: A => B)(g: B => A): Form[B] =
      new Form[B](fa.mapping.transform(f, g), fa.data, fa.errors, fa.value.map(f))
  }

  implicit class RichList[ELEM](inner: List[ELEM]) {
    def replace(ordinal: Int, elem: ELEM): List[ELEM] = 
      if (ordinal >= 0 && ordinal < inner.size) 
        inner.take(ordinal) ++ {elem :: inner.drop(ordinal + 1)}
      else
        throw new IndexOutOfBoundsException

    def delete(ordinal: Int): List[ELEM] =
      if (ordinal >= 0 && ordinal < inner.size) 
        inner.take(ordinal) ++ inner.drop(ordinal + 1)
      else
        throw new IndexOutOfBoundsException
  }
  
}
