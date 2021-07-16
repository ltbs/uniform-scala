package ltbs.uniform

import cats.data.NonEmptyList

/** an error, usually constructed into an ErrorTree to provide the
  * paths to the fields where the errors occurred
  * 
  * {{{
  * val errorTree = ErrorMsg("myform.myfield.badinput").toTree
  * }}}
  */
case class ErrorMsg(msg: String, args: Any*) {

  /** Add a path to an error by prefixing it with dot (.) separated
    * strings 
    */
  def prefixWith(in: List[String]): ErrorMsg =
    ErrorMsg(in.mkString(".") ++ "." ++ msg, args:_*)

  /** Given a messages provider return rendered content for this
    * error 
    */
  def render[A](msgProvider: UniformMessages[A]): A =
    msgProvider.decompose(msg, args:_*)

  /** Create an [[ErrorTree]] for this error alone at the root of
    * the tree 
    */
  def toTree: ErrorTree = {
    ErrorTree.one(NonEmptyList.one(this))
  }
}
