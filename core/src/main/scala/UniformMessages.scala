package ltbs.uniform

import scala.util.parsing.combinator._
import cats.Monoid
import cats.implicits._

/** Content/internationalisation messages to be used by the interpreter. */
trait UniformMessages[A] {

  /** fetch the message for the key, supplying any arguments. Throw an
    * exception if it not found
    */
  def apply(key: String, args: Any*): A =
    get(key, args:_*).getOrElse(
      throw new NoSuchElementException(s"key not found: $key")
    )

  /** fetch a message for the list of keys, supplying any arguments
    * against the first match. Throw an exception if none of the keys
    * are found
    */
  def apply(keys: List[String], args: Any*): A =
    get(keys, args:_*).getOrElse(throw new NoSuchElementException(
      s"""keys not found: ${keys.mkString(",")}"""
    ))

  /** fetch the message for the key, supplying any arguments. */
  def get(key: String, args: Any*): Option[A]

  /** fetch a message for the list of keys, supplying any arguments
    * against the first match. 
    */  
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

  /** fetch the messages for the key, supplying any arguments. Expect
    * multiple entries. 
    */
  def list(key: String, args: Any*): List[A]

  /** The same as decompose, except returning an None rather than an
    * exception in the event of there being no match 
    */
  def decomposeOpt(key: String, args: Any*): Option[A] = 
    get(
      key.split("[.]").tails.collect{
        case c if c.nonEmpty => c.mkString(".")
      }.toList,
      args:_*)

  /** search for the 'tails' of the keys split by full-stops. For
    * example if the supplied key is 'a.b.c' a match for 'a.b.c' would
    * be preferred, followed by 'b.c', followed by just 'c'. This is
    * used to allow general messages - such as
    * 'deliveryaddress.postcode' matching against 'postcode'. Will
    * throw an exception if there are no matches against any of the
    * keys 
    */
  def decompose(key: String, args: Any*): A =
    decomposeOpt(key, args:_*).getOrElse(
      throw new NoSuchElementException(s"""key not found: $key""")
    )

  /** create a new UniformMessages based on the existing one. Uses a
    * fallback function to provide the content in the event of no
    * match being found 
    */
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

  /** Produce a simple UniformMessages from a map. Any arguments
    * supplied are ignored. 
    */
  def fromMap[A](msg: Map[String,List[A]]): UniformMessages[A] =
    SimpleMapMessages[A](msg)

  /** Produce a simple UniformMessages from a map. Substitutes
    * arguments similiarly to Play Messages.
    */
  def fromMapWithSubstitutions(
    msg: Map[String,List[String]]
  ): UniformMessages[String] = MapMessagesWithSubstitutions(msg)

  /** A messages provider that has no values. */
  def noop[A]: UniformMessages[A] = NoopMessages[A]

  /** A messages provider that will echo the keys back in the event of
    * being asked for a mandatory (apply) value. Will return None for
    * non-mandatory values. 
    */
  def echo: UniformMessages[String] = new UniformMessages[String] {
    def get(key: String, args: Any*): Option[String] = None
    def list(key: String, args: Any*): List[String] = Nil
    override def apply(key: String, args: Any*): String = key
    override def apply(keys: List[String], args: Any*): String = keys.head
    override def decompose(key: String, args: Any*): String = key
  }

  /** A messages provider that will echo the key back, even if asked
    * for an optional value 
    */
  def attentionSeeker: UniformMessages[String] = new UniformMessages[String] {
    def get(key: String, args: Any*): Option[String] = Some(key)
    def list(key: String, args: Any*): List[String] = List(key)
    override def apply(key: String, args: Any*): String = key
    override def apply(keys: List[String], args: Any*): String = keys.head
    override def decompose(key: String, args: Any*): String = key
  }

  /** A messages provider that attempts to deconstruct a keyword into
    * a sentence and apply sensible capitalisation for non-optional
    * values. 
    * 
    * For example 'get('yetAnotherField')' would yield 'Get Another Field'
    */
  def bestGuess: UniformMessages[String] = BestGuessMessages

  implicit def contentMonoidInstance[A] = new Monoid[UniformMessages[A]] {
    def empty: UniformMessages[A] = NoopMessages[A]
    def combine(a: UniformMessages[A], b: UniformMessages[A]):UniformMessages[A] = new UniformMessages[A] {
      def get(key: String, args: Any*): Option[A] = a.get(key, args:_*).orElse(b.get(key, args:_*))
      override def get(key: List[String], args: Any*): Option[A] = a.get(key, args:_*).orElse(b.get(key, args:_*))
      def list(key: String, args: Any*): List[A] = a.list(key, args:_*) |+| b.list(key, args:_*)
      override def decompose(key: String, args: Any*): A = a.decomposeOpt(key, args:_*).getOrElse(b.decompose(key, args:_*))

      override def apply(key: String, args: Any*): A = 
         a.get(key, args:_*).getOrElse(b(key, args:_*))

      override def apply(keys: List[String], args: Any*): A =
         a.get(keys, args:_*).getOrElse(b(keys, args:_*))        
    }
  }
}

case class SimpleMapMessages[A](msg: Map[String,List[A]]) extends UniformMessages[A] {
  def get(key: String, args: Any*): Option[A] = list(key, args:_*).headOption
  def list(key: String, args: Any*): List[A] = msg.get(key).getOrElse(Nil)
}

case class MapMessagesWithSubstitutions(underlying: Map[String,List[String]]) extends UniformMessages[String] {

  @annotation.tailrec
  private def replaceArgs(
    input: String,
    args: List[String],
    count: Int = 0
  ): String = args match {
    case Nil    => input
    case h :: t => replaceArgs(input.replace(s"[{]$count[}]", h), t, count+1)
  }
  
  def get(key: String, args: Any*): Option[String] =
    underlying.get(key).flatMap{_.headOption}.map{
      replaceArgs(_,args.toList.map(_.toString))
    }

  override def get(key: List[String], args: Any*): Option[String] = {

    @annotation.tailrec
    def inner(innerkey: List[String]): Option[String] = {
      innerkey match {
        case Nil => None
        case x::xs =>
          get(x, args:_*) match {
            case Some(o) => Some(o)
            case None => inner(xs)
          }
      }
    }
    inner(key)
  }


  def list(key: String, args: Any*): List[String] =
    underlying.getOrElse(key,Nil).map{ x =>
      replaceArgs(x,args.toList.map(_.toString))
    }
}

case class NoopMessages[A]() extends UniformMessages[A] {
  def get(key: String, args: Any*): Option[A] = None
  def list(key: String, args: Any*): List[A] = Nil
}

case class EmptyMessages[A]()(implicit mon: Monoid[A]) extends UniformMessages[A] {

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
      case Failure(_,_) => key
      case Error(_,_) =>   key
    }
  }
}
