package ltbs.uniform
package validation

import cats.data.Validated
import cats.syntax.monoid._

case class FollowedByRule[A] protected (a: Rule[A], b: Rule[A]) extends Rule[A] {
  def apply(value: A): Validated[ErrorTree, A] = a(value) andThen b
}

case class AlongWithRule[A] protected (a: Rule[A], b: Rule[A]) extends Rule[A] {
  def apply(value: A): Validated[ErrorTree, A] = {a |+| b}.apply(value)
}

