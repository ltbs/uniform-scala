package ltbs.uniform

import cats.implicits._
import cats.Monoid
import cats.data.NonEmptyList

case class ErrorMsg(msg: String, args: Any*) {

  def prefixWith(in: List[String]): ErrorMsg =
    ErrorMsg(in.mkString(".") ++ "." ++ msg, args:_*)

  def render[A](msgProvider: UniformMessages[A]): A =
    msgProvider(msg, args:_*)

  def toTree: ErrorTree = {
    ErrorTree.one(NonEmptyList.one(this))
  }
}

trait Rule[A] {

  def apply(in: A): ErrorTree

  def either(in: A): Either[ErrorTree, A] = apply(in) match {
    case ErrorTree.empty ⇒ Right(in)
    case x ⇒ Left(x)
  }

  def andThen(that: Rule[A]): Rule[A] = {
    val orig = this
    new Rule[A] {
      def apply(in: A): ErrorTree = orig.apply(in) match {
        case ErrorTree.empty ⇒ that.apply(in)
        case xs  ⇒ xs
      }
    }
  }
}

object Rule {

  def assert[A](pred: A ⇒ Boolean)(error: ErrorMsg, pathH: InputPath, pathT: InputPath*) = {
    val paths = NonEmptyList(pathH,pathT.toList)
    new Rule[A] {
      def apply(in: A) = if (pred(in))
        ErrorTree.empty
      else
        Map(paths → NonEmptyList.one(error))
    }
  }

  def apply[A](rules: (A ⇒ Boolean, (ErrorMsg, NonEmptyList[InputPath]))*): List[List[Rule[A]]] =
    List(rules.toList.map{case (r, msg) ⇒ fromPred(r, msg)})

  def fromPred[A](pred: A ⇒ Boolean, msg: (ErrorMsg, NonEmptyList[InputPath])): Rule[A] = new Rule[A] {
    def apply(in: A) = if (pred(in)) ErrorTree.empty else Map(msg._2 → NonEmptyList.one(msg._1))
  }

  def pattern[A](pattern: PartialFunction[A,(ErrorMsg, NonEmptyList[InputPath])]): Rule[A] = new Rule[A] {
    val f = pattern.andThen{
      case (msg, paths) ⇒ Map(paths → NonEmptyList.one(msg))
    }
    def apply(in: A) = f.applyOrElse(in, {_: A ⇒ ErrorTree.empty})
  }

  implicit def vrMonoid[A]: Monoid[Rule[A]] = new Monoid[Rule[A]] {
    def empty: Rule[A] = noop
    def combine(x: Rule[A], y: Rule[A]) = new Rule[A] {
      def apply(in: A): ErrorTree = x.apply(in) |+| y.apply(in)
    }
  }

  def alwaysFail[A]: Rule[A] = new Rule[A] {
    def apply(in: A): ErrorTree = ErrorTree.oneErr(ErrorMsg("none-shall-pass"))
  }

  def noop[A]: Rule[A] = new Rule[A] {
    def apply(in: A): ErrorTree = ErrorTree.empty
  }

  def size[A: Quantity](min: Int, max: Int) = QuantityRule[A](min, max)
  def min[A: Quantity](min: Int) = QuantityRule[A](min, Int.MaxValue)
  def max[A: Quantity](max: Int) = QuantityRule[A](0, max)

}
