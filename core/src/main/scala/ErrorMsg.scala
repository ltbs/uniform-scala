package ltbs.uniform

import cats.data.NonEmptyList

/** an error, usually constructed into an ErrorTree to provide the
  * paths to the fields where the errors occurred
  */
case class ErrorMsg(msg: String, args: Any*) {

  def prefixWith(in: List[String]): ErrorMsg =
    ErrorMsg(in.mkString(".") ++ "." ++ msg, args:_*)

  def render[A](msgProvider: UniformMessages[A]): A =
    msgProvider.decompose(msg, args:_*)

  def toTree: ErrorTree = {
    ErrorTree.one(NonEmptyList.one(this))
  }
}
