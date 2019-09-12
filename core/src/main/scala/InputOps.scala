package ltbs.uniform

import cats.data.Validated
import cats.{Monoid, Eq, Order}

trait InputOps {

  def test[A](predicate: A => Boolean, errorMsg: String, args: Any*): A => Validated[ErrorTree, A] = {
    value : A =>
    Validated.cond(predicate(value), value, ErrorTree.oneErr(ErrorMsg(errorMsg, args:_*)))
  }

  def minLength(len: Int) = 
    test[String](_.length >= len, "limit", len)

  def maxLength(len: Int) = 
    test[String](_.length <= len, "limit", len)

  def lengthBetween(min: Int, max: Int): String => Validated[ErrorTree, String] =
    minLength(min)(_) andThen maxLength(max)    

  def matchesRegex(regex: String) =
    test[String](_.matches(regex), "invalid")

  def nonEmpty[A: Monoid: Eq](value: A) =
    test[A](!Monoid[A].isEmpty(_), "required")(value)

  def nonEmptyString =
    test[String](_.nonEmpty, "required")

  def min[A: Order](minValue: A)(value: A) = {
    import cats.syntax.order._
    test[A](minValue <= _, "too-low")(value)
  }

  def max[A: Order](maxValue: A)(value: A) = {
    import cats.syntax.order._
    test[A](_ <= maxValue, "too-high")(value)
  }

  def between[A: Order](minValue: A, maxValue: A): A => Validated[ErrorTree, A] =
    min[A](minValue)(_) andThen max[A](maxValue)
  
}
