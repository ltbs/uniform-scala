package ltbs.uniform
package validation

import cats.implicits._
import cats.{Eq, Monoid, Order}
import cats.data.Validated

object Rule extends Quantifiable.ToQuantifiableOps {

  def error(errorMsg: String, args: Any*): ErrorTree =
    ErrorTree.oneErr(ErrorMsg(errorMsg, args:_*))

  def condError[A](predicate: A => Boolean, errorTree: ErrorTree): Rule[A] = {
    a: A => Validated.cond(predicate(a), a, errorTree)
  }

  def cond[A](predicate: A => Boolean, errorMsg: String, args: Any*): Rule[A] =
    condError(predicate, error(errorMsg, args: _*))

  def condAtPath[A](pathHead: String, pathTail: String*)(predicate: A => Boolean, errorMsg: String, args: Any*): Rule[A] =
    condError(predicate, error(errorMsg, args: _*).atPath(pathHead :: pathTail.toList))

  case class minLength[A: Quantifiable](len: Int, errorMsg: String = "minLength") extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in.qty >= len, in, error(errorMsg, in.qty, len))
  }

  case class maxLength[A: Quantifiable](len: Int, errorMsg: String = "maxLength") extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in.qty <= len, in, error(errorMsg, in.qty, len))
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
      Validated.cond(!in.isEmpty, in, error("required"))
  }

  case class matchesRegex(regex: String) extends Rule[String] {
    def apply(in: String): Validated[ErrorTree, String] =
      Validated.cond(in matches regex, in, error("format"))
  }

  case class min[A: Order](minValue: A) extends Rule[A]{
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in >= minValue, in, error("min"))
  }

  case class max[A: Order](maxValue: A) extends Rule[A]{
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in <= maxValue, in, error("max"))
  }

  case class between[A: Order](minValue: A, maxValue: A) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      min(minValue).apply(in) andThen (max(maxValue).apply(_))
  }
}
