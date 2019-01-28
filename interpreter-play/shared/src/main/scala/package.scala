package ltbs.uniform.interpreters

import cats.data._
import cats.Invariant
import play.api.data.Form
import play.api.mvc.{ Request, AnyContent }
import play.twirl.api.Html
import ltbs.uniform._

package object playframework {

  type PlayForm[A] = SimpleInteractionForm[Request[AnyContent],A,Html]

  type Encoded = String
  type DB = Map[String,Encoded]
  type ValidationError = String
  type ValidatedData[A] = Option[Validated[ValidationError, A]]

  implicit val playFormFunctor: Invariant[Form] = new Invariant[Form]{
    def imap[A, B](fa: Form[A])(f: A => B)(g: B => A): Form[B] =
      new Form[B](fa.mapping.transform(f, g), fa.data, fa.errors, fa.value.map(f))
  }
}
