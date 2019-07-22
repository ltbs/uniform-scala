package ltbs.uniform

import cats.implicits._
import cats.Monoid
import cats.data.NonEmptyList

/** an error, usually constructed into an ErrorTree to provide the
  * paths to the fields where the errors occurred 
  */
case class ErrorMsg(msg: String, args: Any*) {

  def prefixWith(in: List[String]): ErrorMsg =
    ErrorMsg(in.mkString(".") ++ "." ++ msg, args:_*)

  def render[A](msgProvider: UniformMessages[A]): A =
    msgProvider(msg, args:_*)

  def toTree: ErrorTree = {
    ErrorTree.one(NonEmptyList.one(this))
  }
}

/** A validation rule used to check input data. */
trait Rule[A] {

  /** give a tree of errors for the input data. ErrorTree.empty is
    * returned if the data is valid 
    */
  def apply(in: A): ErrorTree

  /** check the input and return a Left(ErrorTree) if there is an
    * error or a Right(a) if the data is valid 
    */
  def either(in: A): Either[ErrorTree, A] = apply(in) match {
    case ErrorTree.empty => Right(in)
    case x => Left(x)
  }

  /** compose a new validation rule by chaining two together */
  def andThen(that: Rule[A]): Rule[A] = {
    val orig = this
    new Rule[A] {
      def apply(in: A): ErrorTree = orig.apply(in) match {
        case ErrorTree.empty => that.apply(in)
        case xs  => xs
      }
    }
  }
}

object Rule {

  def assert[A](pred: A => Boolean)(error: ErrorMsg, pathH: InputPath, pathT: InputPath*) = {
    val paths = NonEmptyList(pathH,pathT.toList)
    new Rule[A] {
      def apply(in: A) = if (pred(in))
        ErrorTree.empty
      else
        Map(paths -> NonEmptyList.one(error))
    }
  }

  def apply[A](rules: (A => Boolean, (ErrorMsg, NonEmptyList[InputPath]))*): List[List[Rule[A]]] = 
    List(rules.toList.map{case (r, msg) => fromPred(r, msg)})

  def fromPred[A](pred: A => Boolean, msg: (ErrorMsg, NonEmptyList[InputPath])): Rule[A] = new Rule[A] {
    def apply(in: A) = if (pred(in)) ErrorTree.empty else Map(msg._2 -> NonEmptyList.one(msg._1))
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

}
