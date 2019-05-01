package ltbs.uniform

import cats.implicits._
import cats.Monoid
import cats.data.ValidatedNel

case class ErrorMsg(msg: String, args: Any*) {
  def render[A](msgProvider: UniformMessages[A]): A =
    msgProvider(msg, args:_*)
}

trait ValidationRule[A] {

  def errorsFor(in: A): List[ErrorMsg]
  def apply(in: A): ValidatedNel[ErrorMsg, A] = errorsFor(in).toNel match {
    case None => in.valid
    case Some(nel) => nel.invalid
  }
  def andThen(that: ValidationRule[A]): ValidationRule[A] = {
    val orig = this
    new ValidationRule[A] {
      def errorsFor(in: A): List[ErrorMsg] = orig.errorsFor(in) match {
        case Nil => that.errorsFor(in)
        case xs  => xs
      }
    }
  }
}

case class ListValidation[A](
  elementValidation: ValidationRule[A]
) extends ValidationRule[List[A]] {
  def errorsFor(in: List[A]) = in.flatMap(elementValidation.errorsFor)
}

object ValidationRule {

  def apply[A](rules: (A => Boolean, ErrorMsg)*): List[List[ValidationRule[A]]] = 
    List(rules.toList.map{case (r, msg) => fromPred(r, msg)})

  def fromPred[A](pred: A => Boolean, msg: ErrorMsg): ValidationRule[A] = new ValidationRule[A] {
    def errorsFor(in: A) = if (pred(in)) Nil else List(msg)
  }

  implicit def vrMonoid[A]: Monoid[ValidationRule[A]] = new Monoid[ValidationRule[A]] {
    def empty: ValidationRule[A] = new ValidationRule[A] {
      def errorsFor(in: A): List[ErrorMsg] = Nil
    }
    def combine(x: ValidationRule[A], y: ValidationRule[A]) = new ValidationRule[A] {
      def errorsFor(in: A): List[ErrorMsg] = x.errorsFor(in) |+| y.errorsFor(in)
    }
  }

  def alwaysFail[A]: ValidationRule[A] = new ValidationRule[A] {
    def errorsFor(in: A): List[ErrorMsg] = ErrorMsg("none-shall-pass") :: Nil
  }

}
