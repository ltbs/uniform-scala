package ltbs.uniform

import scala.util.parsing.combinator._
import cats.Monoid

trait UniformMessages[A] {
  def apply(key: String, args: Any*): A =
    get(key, args:_*).getOrElse(throw new NoSuchElementException(s"key not found: $key"))
  def apply(keys: List[String], args: Any*): A =
    get(keys, args:_*).getOrElse(throw new NoSuchElementException(s"""keys not found: ${keys.mkString(",")}"""))    
  def get(key: String, args: Any*): Option[A]
  def get(keys: List[String], args: Any*): Option[A] = {
    @annotation.tailrec
    def inner(keys: List[String], args: Seq[Any]): Option[A] = keys match {
      case Nil     => None
      case (k::ks) => get(k, args:_*) match {
        case Some(string) => Some(string)
        case None         => inner(ks, args)
      }
    }
    inner(keys, args)
  }

  def list(key: String, args: Any*): List[A]

  def decomposeOpt(key: String, args: Any*): Option[A] = 
    get(
      key.split("[.]").tails.collect{
        case c if c.nonEmpty => c.mkString(".")
      }.toList,
      args:_*)

  def decompose(key: String, args: Any*): A =
    decomposeOpt(key, args:_*).getOrElse(
      throw new NoSuchElementException(s"""key not found: $key""")
    )

  def withDefault(f: String => A): UniformMessages[A] = {
    val underlying = this
    new UniformMessages[A] {
      override def apply(key: String, args: Any*): A = {
        underlying.get(key, args:_*).getOrElse(f(key))
      }
      override def apply(keys: List[String], args: Any*): A =
        underlying.get(keys, args:_*).getOrElse(f(keys.head))        
      def get(key: String, args: Any*): Option[A] =
        underlying.get(key, args:_*)
      def list(key: String, args: Any*): List[A] = 
        underlying.list(key, args:_*)

      override def decompose(key: String, args: Any*): A =
        underlying.decomposeOpt(key, args:_*).getOrElse(f(key))      
    }
  }

  def map[B](f: A => B): UniformMessages[B] = {
    val underlying = this
    new UniformMessages[B] {
      override def apply(key: String, args: Any*): B =
        f(underlying.apply(key, args:_*))
      override def apply(keys: List[String], args: Any*): B =
        f(underlying.apply(keys, args:_*))
      def get(key: String, args: Any*): Option[B] =
        underlying.get(key, args:_*).map(f)
      override def get(keys: List[String], args: Any*): Option[B] =
        underlying.get(keys, args:_*).map(f)
      def list(key: String, args: Any*): List[B] = 
        underlying.list(key, args:_*).map(f)

      override def decompose(key: String, args: Any*): B =
        f(underlying.decompose(key, args:_*))
    }
  }
}

object UniformMessages {
  def fromMap[A](msg: Map[String,List[A]]): UniformMessages[A] = SimpleMapMessages[A](msg)
  def noop[A]: UniformMessages[A] = NoopMessages[A]
  def echo: UniformMessages[String] = new UniformMessages[String] {
    def get(key: String, args: Any*): Option[String] = None
    def list(key: String, args: Any*): List[String] = Nil
    override def apply(key: String, args: Any*): String = key
    override def apply(keys: List[String], args: Any*): String = keys.head
    override def decompose(key: String, args: Any*): String = key
  }

  def attentionSeeker: UniformMessages[String] = new UniformMessages[String] {
    def get(key: String, args: Any*): Option[String] = Some(key)
    def list(key: String, args: Any*): List[String] = List(key)
    override def apply(key: String, args: Any*): String = key
    override def apply(keys: List[String], args: Any*): String = keys.head
    override def decompose(key: String, args: Any*): String = key
  }

  def bestGuess: UniformMessages[String] = BestGuessMessages
}

case class SimpleMapMessages[A](msg: Map[String,List[A]]) extends UniformMessages[A] {
  def get(key: String, args: Any*): Option[A] = list(key, args:_*).headOption
  def list(key: String, args: Any*): List[A] = msg.get(key).getOrElse(Nil)
}

case class NoopMessages[A]() extends UniformMessages[A] {
  def get(key: String, args: Any*): Option[A] = None
  def list(key: String, args: Any*): List[A] = Nil
}

case class EmptyMessages[A](implicit mon: Monoid[A]) extends UniformMessages[A] {

  override def apply(key: String, args: Any*): A =
    mon.empty
  override def apply(keys: List[String], args: Any*): A =
    mon.empty
  
  def get(key: String, args: Any*): Option[A] = None
  def list(key: String, args: Any*): List[A] = Nil
}


object BestGuessMessages extends RegexParsers with UniformMessages[String] {

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
