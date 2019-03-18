package ltbs.uniform.web

import scala.util.parsing.combinator._

trait Messages {
  def apply(key: String, args: Any*): String =
    get(key, args:_*).getOrElse(key)
  def apply(key: List[String], args: Any*): String =
    get(key, args:_*).getOrElse(key.head)    
  def get(key: String, args: Any*): Option[String]
  def get(key: List[String], args: Any*): Option[String]
  def list(key: String, args: Any*): List[String]

  def decomposeOpt(key: String, args: Any*): Option[String] = 
    get(
      key.split("[.]").tails.collect{
        case c if c.nonEmpty => c.mkString(".")
      }.toList,
      args:_*)

  def decompose(key: String, args: Any*): String =
    apply(
      key.split("[.]").tails.collect{
        case c if c.nonEmpty => c.mkString(".")
      }.toList,
      args:_*)

}

object NoopMessages extends Messages {
  def get(key: String, args: Any*): Option[String] = None
  def get(key: List[String], args: Any*): Option[String] = None  
  def list(key: String, args: Any*): List[String] = Nil
}

case class NeverEmptyMessages(inner: Messages) extends Messages {

  override def apply(key: String, args: Any*): String =
    inner.apply(key, args: _*)

  override def apply(key: List[String], args: Any*): String =
    inner.apply(key, args: _*)

  def get(key: String, args: Any*): Option[String] = 
    Some(inner.apply(key, args: _*))

  def get(key: List[String], args: Any*): Option[String] =
    Some(inner.apply(key, args: _*))

  def list(key: String, args: Any*): List[String] = {
    inner.list(key, args: _*) match {
      case Nil => List(key)
      case xs => xs
    }
  }

  override def decomposeOpt(key: String, args: Any*): Option[String] =
    Some(inner.decompose(key,args:_*))

  override def decompose(key: String, args: Any*): String =
    inner.decompose(key,args:_*)    
  
}

case class BestGuessMessages(inner: Messages) extends RegexParsers with Messages {

  override def apply(key: String, args: Any*): String =
    inner.get(key, args:_*).filter(_ != key).getOrElse(bestGuess(key))

  override def apply(key: List[String], args: Any*): String = key.collectFirst{
    case x if get(x,args:_*).isDefined => apply(x,args:_*)
  }.getOrElse(bestGuess(key.head))

  def get(key: String, args: Any*): Option[String] = inner.get(key, args:_*)
  def get(key: List[String], args: Any*): Option[String] = inner.get(key, args:_*)  

  def list(key: String, args: Any*): List[String] = inner.list(key, args:_*)

  def titleWord: Parser[String]    = """[A-Z][a-z]+""".r ^^ { _.toString }
  def lowerWord: Parser[String]    = """[a-z]+""".r ^^ { _.toString }
  def numberS: Parser[String]    = """([0-9]+)""".r ^^ { _.toString }
  def camel: Parser[List[String]] = phrase(rep1(titleWord | lowerWord | numberS))

  def titleCase(word: String) = word match {
    case "" => ""
    case a => a.head.toUpper + a.tail
  }

  private def bestGuess(key: String): String = {
    parse(camel,key.replaceFirst("[.](heading|option)$","").split("[.]").last) match {
      case Success(Nil,_) => key
      case Success((firstWord::rest),_) => (titleCase(firstWord) :: rest).mkString(" ")
      case Failure(msg,_) =>
        println("FAILURE: " + msg)
        key
      case Error(msg,_) =>
        println("ERROR: " + msg)
        key
    }

  }

}
