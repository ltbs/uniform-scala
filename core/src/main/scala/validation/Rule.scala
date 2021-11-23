package ltbs.uniform
package validation

import cats.implicits._
import cats.Order
import cats.data.Validated

object Rule extends Quantifiable.ToQuantifiableOps {

  def error(errorMsg: String, args: Any*): ErrorTree =
    ErrorTree.oneErr(ErrorMsg(errorMsg, args:_*))

  /** A custom rule from a predicate and an error tree */  
  def condError[A](predicate: A => Boolean, errorTree: ErrorTree): Rule[A] = {
    a: A => Validated.cond(predicate(a), a, errorTree)
  }

  /** A custom rule from a predicate and an error message */
  def cond[A](predicate: A => Boolean, errorMsg: String, args: Any*): Rule[A] =
    condError(predicate, error(errorMsg, args: _*))

  /** A custom rule from a predicate and an error message to be
    * applied to a supplied path */
  def condAtPath[A](pathHead: String, pathTail: String*)(predicate: A => Boolean, errorMsg: String, args: Any*): Rule[A] =
    condError(predicate, error(errorMsg, args: _*).prefixWithMany(pathHead :: pathTail.toList))

  case class MinLength[A: Quantifiable] private (len: Int, errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in.qty >= len, in, error(errorMsg, in.qty, len))
  }

  /** Assert that there are at least `len` elements */  
  def minLength[A: Quantifiable](len: Int, errorMsg: String = "minLength"): Rule[A] =
    MinLength[A](len, errorMsg)

  case class MaxLength[A: Quantifiable] private (len: Int, errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in.qty <= len, in, error(errorMsg, in.qty, len))
  }

  /** Assert that there are no more than `len` elements */
  def maxLength[A: Quantifiable](len: Int, errorMsg: String = "maxLength"): Rule[A] =
    MaxLength[A](len, errorMsg)

  case class AlwaysPass[A] private () extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.Valid(in)
  }

  /** The simplest validation rule - accepts any value */
  def alwaysPass[A]: Rule[A] = AlwaysPass[A]()

  case class AlwaysFail[A] private () extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.Invalid(error("impossible-to-continue"))
  }

  /** The simplest validation rule - accepts any value */
  def alwaysFail[A]: Rule[A] = AlwaysFail[A]()


  case class LengthBetween[A: Quantifiable] private (min: Int, max: Int) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      minLength[A](min).apply(in) andThen (maxLength[A](max).apply(_))
  }

  /** Assert that there are at least `len` elements, but no more than `max` */
  def lengthBetween[A: Quantifiable](min: Int, max: Int): Rule[A] = LengthBetween[A](min, max)

  case class NonEmpty[A: Empty] private (errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(!in.isEmpty, in, error(errorMsg))
  }

  /** Assert that the input is not empty (in the sense of a monoid being 'empty') */
  def nonEmpty[A: Empty](errorMsg: String): Rule[A] = NonEmpty[A](errorMsg)

  /** Assert that the input is not empty (in the sense of a monoid being 'empty') */  
  def nonEmpty[A: Empty]: Rule[A] = nonEmpty("required")

  case class MatchesRegex private (regex: String, errorMsg: String) extends Rule[String] {
    def apply(in: String): Validated[ErrorTree, String] =
      Validated.cond(in matches regex, in, error(errorMsg))
  }

  /** Assert that the input string matches the given regular expression  */  
  def matchesRegex(regex: String, errorMsg: String = "format"): Rule[String] =
    MatchesRegex(regex, errorMsg)

  case class Min[A: Order] private (minValue: A, errorMsg: String) extends Rule[A]{
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in >= minValue, in, error(errorMsg))
  }

  /** Assert that the input value must come after, or in the same
    * position as, `minValue` if the elements were put in order */
  def min[A: Order](minValue: A, errorMsg: String = "min"): Rule[A] =
    Min[A](minValue, errorMsg)

  case class Max[A: Order] private (maxValue: A, errorMsg: String) extends Rule[A]{
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(in <= maxValue, in, error(errorMsg))
  }

  /** Assert that the input value must come before, or in the same
    * position as, `maxValue` if the elements were put in order */  
  def max[A: Order](maxValue: A, errorMsg: String = "max"): Rule[A] =
    Max[A](maxValue, errorMsg)

  case class Between[A: Order] private (minValue: A, maxValue: A) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      min(minValue).apply(in) andThen (max(maxValue).apply(_))
  }

  /** Assert that the input value must come after, or in the same
    * position as, `minValue` but before or in the same position as
    * `maxValue` if the elements were put in order.
    */
  def between[A: Order](minValue: A, maxValue: A): Rule[A] =
    Between[A](minValue, maxValue)

  case class ForEachInList[A](inner: Rule[A]) extends Rule[List[A]] {
    def apply(in: List[A]): Validated[ErrorTree, List[A]] =
      in.map { inner }.sequence
  }

  def forEachInList[A](inner: Rule[A]): Rule[List[A]] =
    ForEachInList[A](inner)

  case class In[A](allowed: Seq[A], errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(allowed.contains(in), in, error(errorMsg))
  }

  def in[A](allowed: Seq[A], errorMsg: String = "not-in-allowed-list"): Rule[A] =
    In(allowed, errorMsg)

  case class NotIn[A](disallowed: Seq[A], errorMsg: String) extends Rule[A] {
    def apply(in: A): Validated[ErrorTree, A] =
      Validated.cond(!disallowed.contains(in), in, error(errorMsg))
  }

  def notIn[A](disallowed: Seq[A], errorMsg: String = "in-disallowed-list"): Rule[A] =
    NotIn(disallowed, errorMsg)


}
