package ltbs.uniform

import cats.data.NonEmptyList

case class ErrorMsg(msg: String, args: Any*) {

  def prefixWith(in: List[String]): ErrorMsg =
    ErrorMsg(in.mkString(".") ++ "." ++ msg, args:_*)

  def render[A](msgProvider: UniformMessages[A]): A =
    msgProvider.decompose(msg, args:_*)

  def toTree: ErrorTree = {
    ErrorTree.one(NonEmptyList.one(this))
  }
}
