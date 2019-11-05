package ltbs.uniform

import cats.Monoid
import cats.implicits._

/** A content bundle
  * 
  * Typically used for internationalisation but also for customisation
  * of content on given steps of a user-journey. 
  * 
  * This is modelled largely around the `Messages` object in the play
  * framework - albeit with some extra capabilities (such as
  * decomposition), but as such although it can take arguments
  * (`Any*`)it is not typesafe and has no way of knowing ahead-of-time if
  * content is missing.  
  * 
  * Unlike play Messages however, UniformMessages are typed, can be
  * mapped to different types and are also Monoids allowing them to be
  * combined. 
  * 
  * {{{
  * val m = UniformMessages.fromMap(
  *   Map("page1.field2.dateOfBirth" -> List("When were you born?"), 
  *       "dateOfBirth" -> List("Date of birth"))
  * )
  * 
  * scala> m.get("page1.field2.dateOfBirth")
  * res1: Option[String] = Some(When were you born?)
  * 
  * scala> m.decomposeOpt("dateOfBirth")
  * res2: Option[String] = Some(Date of birth)
  * 
  * scala> import cats.implicits._
  * scala> {m |+| UniformMessages.bestGuess}.apply("page1.step2.EnterYourBillingAddress")
  * res3: String = Enter Your Billing Address
  * 
  * scala> case class PoorMansHtml(value: String)
  * defined class PoorMansHtml
  * scala> m.map(PoorMansHtml)
  * res4: ltbs.uniform.UniformMessages[PoorMansHtml] = ...
  * }}}
  */
trait UniformMessages[A] {

  /** Retrieve a single message for the `key`, replacing any arguments with the
    * values supplied. Throws an exception if the message is not
    * found.
    * @param key the lookup identifier for the message
    * @param args any arguments to be supplied, how they are handled
    *   depends upon the `UniformMessages1 implementation, but
    *   generally they are injected into the response. 
    */
  def apply(key: String, args: Any*): A =
    get(key, args:_*).getOrElse(throw new NoSuchElementException(s"key not found: $key"))

  /** Retrieve a single message against the `keys`, replacing any
    * arguments with the values supplied. Throws an exception if no 
    * message is found against any of the keys. If there are multiple
    * matches the first one will be used. 
    * 
    * @param keys lookup identifiers for the message, each is tried in turn
    * @param args any arguments to be supplied, how they are handled
    *   depends upon the `UniformMessages1 implementation, but
    *   generally they are injected into the response. 
    */
  def apply(keys: List[String], args: Any*): A =
    get(keys, args:_*).getOrElse(throw new NoSuchElementException(s"""keys not found: ${keys.mkString(",")}"""))

  /** Retrieve a single message for the `key`, replacing any arguments with the
    * values supplied. Returns `None` if the message is not found.
    * 
    * @param key the lookup identifier for the message
    * @param args any arguments to be supplied, how they are handled
    *   depends upon the `UniformMessages1 implementation, but
    *   generally they are injected into the response. 
    */  
  def get(key: String, args: Any*): Option[A]

  /** Retrieve a single message against the `keys`, replacing any
    * arguments with the values supplied. Returns `None` if no 
    * message is found against any of the keys. If there are multiple
    * matches the first one will be used. 
    * 
    * @param keys lookup identifiers for the message, each is tried in turn
    * @param args any arguments to be supplied, how they are handled
    *   depends upon the `UniformMessages1 implementation, but
    *   generally they are injected into the response. 
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

  /** Retrieve a list of values against a single key, if the key is
    * not found in the messages this will return `Nil`
    */
  def list(key: String, args: Any*): List[A]

  /** Attempt to match against `key`, if a match is found return it,
    * otherwise attempt to find fallback values by splitting the `key`
    * into substrings using '.' (full-stop) as a separator.
    * 
    * {{{
    * val m = UniformMessages.fromMap(
    *   Map("a1.b1.c1" -> List("Match1"), 
    *       "b1.c1"    -> List("Match2"),
    *       "c1"       -> List("Match3"))
    * )
    * scala> m.decomposeOpt("a1.b1.c1")
    * res1: Option[String] = Some(Match1)
    * 
    * scala> m.decomposeOpt("amodified.b1.c1")
    * res2: Option[String] = Some(Match2)
    * 
    * scala> m.decomposeOpt("amodified.bmodified.c1")
    * res3: Option[String] = Some(Match3)
    * 
    * scala> m.decomposeOpt("amodified.bmodified.cmodified")
    * res4: Option[String] = None
    * 
    * scala> m.decomposeOpt("a1.b1.cmodified")
    * res5: Option[String] = None
    * }}}
    * 
    * If still no match is found return `None`.
    */
  def decomposeOpt(key: String, args: Any*): Option[A] =
    get(
      key.split("[.]").tails.collect{
        case c if c.nonEmpty => c.mkString(".")
      }.toList,
      args:_*)

  /** Attempt to match against `key`, if a match is found return it,
    * otherwise attempt to find fallback values by splitting the `key`
    * into substrings using '.' (full-stop) as a separator.
    * 
    * {{{
    * val m = UniformMessages.fromMap(
    *   Map("a1.b1.c1" -> List("Specific"), 
    *       "b1.c1"    -> List("Match2"),
    *       "c1"       -> List("Very General"))
    * )
    * 
    * scala> m.decompose("a1.b1.c1")
    * res1: String = "Specific"
    * 
    * scala> m.decompose("amodified.b1.c1")
    * res2: String = "Match2"
    * 
    * scala> m.decompose("amodified.bmodified.c1")
    * res3: String = "Very General"
    * 
    * scala> m.decompose("nonexistant") // throws exception
    * }}}
    * 
    * If still no match is found an exception is thrown.
    */
  def decompose(key: String, args: Any*): A =
    decomposeOpt(key, args:_*).getOrElse(
      throw new NoSuchElementException(s"""key not found: $key""")
    )

  /** provides another instance using a fallback function.
    * 
    * As a consequence this should never throw a
    * NoSuchElementException.
    * 
    * {{{
    * scala> val m = UniformMessages.noop[Int].withDefault(_.size)
    * m: ltbs.uniform.UniformMessages[Int] = ...
    * 
    * scala> m.get("non-existant")
    * res0: Option[Int] = None
    * 
    * scala> m("non-existant")
    * res1: Int = 12
    * }}}
    * 
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

  def withCustomContent(customContent: Map[String,(String, List[Any])]): UniformMessages[A] = {
    val underlying = this
    UniformMessages.contentMonoidInstance[A].combine(
      new UniformMessages[A] {
        def get(key: String,args: Any*): Option[A] = customContent.get(key) flatMap {
          case (newKey, newArgs) =>
            Either.catchOnly[NoSuchElementException](underlying(newKey, newArgs:_*)).toOption
        }

        def list(key: String,args: Any*): List[A] =
          customContent.get(key).map{
            case (newKey, newArgs) => underlying.list(newKey, newArgs:_*)
          }.getOrElse(Nil)
      },
      underlying
    )
  }
}

object UniformMessages {
  def fromMap[A](msg: Map[String,List[A]]) = new UniformMessages[A] {
    def get(key: String, args: Any*): Option[A] = list(key, args:_*).headOption
    def list(key: String, args: Any*): List[A] = msg.get(key).getOrElse(Nil)
  }

  def fromMapWithSubstitutions(msg: Map[String,List[String]]): UniformMessages[String] = MapMessagesWithSubstitutions(msg)
  def noop[A] = new UniformMessages[A] {
    def get(key: String, args: Any*): Option[A] = None
    def list(key: String, args: Any*): List[A] = Nil
  }

  def empty[A](implicit mon: Monoid[A]) = new UniformMessages[A] {
    override def apply(key: String, args: Any*): A =
      mon.empty
    override def apply(keys: List[String], args: Any*): A =
      mon.empty
    def get(key: String, args: Any*): Option[A] = None
    def list(key: String, args: Any*): List[A] = Nil
  }

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

  implicit def contentMonoidInstance[A] = new Monoid[UniformMessages[A]] {
    def empty: UniformMessages[A] = noop[A]
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
