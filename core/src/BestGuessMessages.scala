package ltbs.uniform

import scala.util.parsing.combinator._

private[uniform] object BestGuessMessages extends RegexParsers with UniformMessages[String] {

  override def apply(key: String, args: Any*): String =
    bestGuess(key)
  override def apply(keys: List[String], args: Any*): String =
    bestGuess(keys.head)

  override def decompose(key: String, args: Any*): String =
    bestGuess(key)

  def get(key: String, args: Any*): Option[String] = None
  def list(key: String, args: Any*): List[String] = Nil
  def titleWord: Parser[String]    = """[A-Z][a-z]+""".r ^^ { _.toString }
  def lowerWord: Parser[String]    = """[a-z]+""".r ^^ { _.toString }
  def numberS: Parser[String]    = """([0-9]+)""".r ^^ { _.toString }
  def camel: Parser[List[String]] = phrase(rep1(opt("-") ~> (titleWord | lowerWord | numberS)))

  def titleCase(word: String) = word match {
    case "" => ""
    case a => s"${a.head.toUpper}${a.tail}"
  }

  private def bestGuess(key: String): String = {
    parse(camel,key.replaceFirst("[.](heading|option)$","").split("[.]").last) match {
      case Success(Nil,_) => key
      case Success((firstWord::rest),_) => (titleCase(firstWord) :: rest).mkString(" ")
      case Failure(_,_) => key
      case Error(_,_) =>   key
    }
  }
}
