package ltbs.uniform

import language.implicitConversions

import cats.implicits._
import cats.Monoid
import cats.data.Validated

import simulacrum._

package object validation extends validation.Compat {
  type Transformation[A, B] = A => Validated[ErrorTree, B]
  type Rule[A] = Transformation[A,A]

  implicit val quantString = Quantifiable.instance[String](x => x.length)

  implicit def ruleMonoidInstance[A] = new Monoid[Rule[A]] {
    def empty: Rule[A] = Rule.alwaysPass[A]()
   
    def combine(x: Rule[A],y: Rule[A]): Rule[A] = new Rule[A] {
      def apply(in: A) = (x.apply(in),y.apply(in)).mapN{case x => x._1}
    }
  }

  implicit class RichTransformation[A, B](trans: Transformation[A, B]) {
    def atPath(p: String, px: String*): Transformation[A, B] =
      trans(_).leftMap(_.atPath(p::px.toList))
    def atPath(p: List[String]): Transformation[A, B] =
      trans(_).leftMap(_.atPath(p))
  }

  implicit class RichRule[A](rule: Rule[A]) {
    def either(in: A): Either[ErrorTree, A] = rule.apply(in).toEither
  }

}

package validation {

  @typeclass trait Quantifiable[A] {
    def qty(in: A): Int
  }

  object Quantifiable {
    def instance[A](f: A => Int) = new Quantifiable[A] {
      def qty(in: A): Int = f(in)
    }
  }
}
