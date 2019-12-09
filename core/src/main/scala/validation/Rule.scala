package ltbs.uniform
package validation

import cats.implicits._
import cats.Order
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
    condError(predicate, error(errorMsg, args: _*).prefixWithMany(pathHead :: pathTail.toList))

  case class MinLength[A: Quantifiable](len: Int, errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in.qty >= len, in, error(errorMsg, in.qty, len))
  }
  def minLength[A: Quantifiable](len: Int, errorMsg: String = "minLength"): Rule[A] =
    MinLength[A](len, errorMsg)

  case class MaxLength[A: Quantifiable](len: Int, errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in.qty <= len, in, error(errorMsg, in.qty, len))
  }
  def maxLength[A: Quantifiable](len: Int, errorMsg: String = "maxLength"): Rule[A] =
    MaxLength[A](len, errorMsg)

  case class AlwaysPass[A]() extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.Valid(in)
  }
  def alwaysPass[A]: Rule[A] = AlwaysPass[A]()

  case class LengthBetween[A: Quantifiable](min: Int, max: Int) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      minLength[A](min).apply(in) andThen (maxLength[A](max).apply(_))
  }
  def lengthBetween[A: Quantifiable](min: Int, max: Int): Rule[A] = LengthBetween[A](min, max)

  case class NonEmpty[A: Empty](errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(!in.isEmpty, in, error(errorMsg))
  }

  def nonEmpty[A: Empty](errorMsg: String): Rule[A] = NonEmpty[A](errorMsg)
  def nonEmpty[A: Empty]: Rule[A] = nonEmpty("required")

  case class MatchesRegex(regex: String, errorMsg: String) extends Rule[String] {
    def apply(in: String): Validated[ErrorTree, String] =
      Validated.cond(in matches regex, in, error(errorMsg))
  }
  def matchesRegex(regex: String, errorMsg: String = "format"): Rule[String] =
    MatchesRegex(regex, errorMsg)

  case class Min[A: Order](minValue: A, errorMsg: String) extends Rule[A]{
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in >= minValue, in, error(errorMsg))
  }
  def min[A: Order](minValue: A, errorMsg: String = "min"): Rule[A] =
    Min[A](minValue, errorMsg)

  case class Max[A: Order](maxValue: A, errorMsg: String = "max") extends Rule[A]{
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in <= maxValue, in, error(errorMsg))
  }
  def max[A: Order](maxValue: A, errorMsg: String = "max"): Rule[A] =
    Max[A](maxValue, errorMsg)

  case class Between[A: Order](minValue: A, maxValue: A) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      min(minValue).apply(in) andThen (max(maxValue).apply(_))
  }
  def between[A: Order](minValue: A, maxValue: A): Rule[A] =
    Between[A](minValue, maxValue)

  case class ForEachInList[A](inner: Rule[A]) extends Rule[List[A]] {
    def apply(in: List[A]): Validated[ErrorTree, List[A]] =
      in.map { inner }.sequence
  }

  def forEachInList[A](inner: Rule[A]): Rule[List[A]] =
    ForEachInList[A](inner)
}
