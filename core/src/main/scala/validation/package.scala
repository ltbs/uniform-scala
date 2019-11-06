package ltbs.uniform

import cats.implicits._
import cats.Monoid
import cats.data.Validated

package object validation
    extends validation.Compat
    with Quantifiable.ToQuantifiableOps
    with QuantifiableInstances
    with Empty.ToEmptyOps
    with EmptyInstances
{
  type Transformation[A, B] = A => Validated[ErrorTree, B]
  type Rule[A] = Transformation[A,A]

  implicit def ruleMonoidInstance[A] = new Monoid[Rule[A]] {
    def empty: Rule[A] = Rule.alwaysPass[A]
   
    def combine(x: Rule[A],y: Rule[A]): Rule[A] = new Rule[A] {
      import Validated.{Valid, Invalid}
      def apply(in: A) = (x.apply(in),y.apply(in)) match {
        case (Valid(_), Valid(_)) => Valid(in)
        case (Invalid(e), Valid(_)) => Invalid(e)
        case (Valid(_), Invalid(e)) => Invalid(e)
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
      }
    }
  }

  implicit class RichRule[A](rule: Rule[A]) {
    def either(in: A): Either[ErrorTree, A] = rule.apply(in).toEither
    def followedBy(ruleB: Rule[A]): Rule[A] = rule(_) andThen ruleB
  }
}
