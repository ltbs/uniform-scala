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

    /** compose a new validation rule by chaining two together in
      * sequence. The second Rule will not be executed unless the
      * first passes.
      */
    def followedBy(ruleB: Rule[A]): Rule[A] = FollowedByRule(rule, ruleB)

    /** compose a new validation rule by running two together in
      * parallel. The second Rule will be executed regardless of 
      * if the first passes and any errors aggregated.
      */
    def alongWith(ruleB: Rule[A]): Rule[A] = AlongWithRule(rule, ruleB)    

    /** In the event that the validation is some form of aggregation
      * (such as [[FollowedByRule]] or [[AlongWithRule]]) split it out into all of 
      * its non-aggregate components. 
      * 
      * If the rule is a normal rule return a singleton list with that rule. 
      */
    def subRules: List[Rule[A]] = {
      @annotation.tailrec
      def inner(normal: List[Rule[A]], unsorted: List[Rule[A]]): List[Rule[A]] = {
        unsorted match {
          case (FollowedByRule(a,b)::xs) => inner(normal, a :: b :: xs)
          case (AlongWithRule(a,b)::xs) => inner(normal, a :: b :: xs)
          case x::xs => inner(x::normal, xs)
          case Nil => normal
        }
      }

      inner(Nil, List(rule))
    }
  }
}
