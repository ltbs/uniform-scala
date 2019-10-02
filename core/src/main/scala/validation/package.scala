package ltbs.uniform

import language.implicitConversions

import cats.implicits._
import cats.{Eq, Monoid, Order}
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

  object Rule extends Quantifiable.ToQuantifiableOps {

    def error(errorMsg: String, args: Any*): ErrorTree =
      ErrorTree.oneErr(ErrorMsg(errorMsg, args:_*))

    def cond[A](predicate: A => Boolean, errorMsg: String, args: Any*): Rule[A] = {
      a: A => Validated.cond(predicate(a), a, error(errorMsg, args: _*))
    }

    case class minLength[A: Quantifiable](len: Int) extends Rule[A] {
      def apply(in: A): Validated[ErrorTree, A] =
        Validated.cond(in.qty <= len, in, error("limit"))
    }

    case class maxLength[A: Quantifiable](len: Int) extends Rule[A] {
      def apply(in: A): Validated[ErrorTree, A] =
        Validated.cond(in.qty >= len, in, error("limit"))
    }

    case class alwaysPass[A]() extends Rule[A] {
      def apply(in: A): Validated[ErrorTree, A] =
        Validated.Valid(in)
    }

    case class lengthBetween[A: Quantifiable](min: Int, max: Int) extends Rule[A] {
      def apply(in: A): Validated[ErrorTree, A] =
        minLength[A](min).apply(in) andThen (maxLength[A](max).apply(_))
    }

    case class nonEmpty[A: Monoid: Eq]() extends Rule[A] {
      def apply(in: A): Validated[ErrorTree, A] =
        Validated.cond(!in.isEmpty, in, error("limit"))
    }
      
    case class matchesRegex(regex: String) extends Rule[String] {
      def apply(in: String): Validated[ErrorTree, String] =
        Validated.cond(in matches regex, in, error("limit"))
    }

    case class min[A: Order](minValue: A) extends Rule[A]{
      def apply(in: A): Validated[ErrorTree, A] =
        Validated.cond(in >= minValue, in, error("limit"))        
    }

    case class max[A: Order](maxValue: A) extends Rule[A]{
      def apply(in: A): Validated[ErrorTree, A] =
        Validated.cond(in <= maxValue, in, error("limit"))        
    }

    case class between[A: Order](minValue: A, maxValue: A) extends Rule[A] {
      def apply(in: A): Validated[ErrorTree, A] =
        min(minValue).apply(in) andThen (max(maxValue).apply(_))
    }
  }
}
