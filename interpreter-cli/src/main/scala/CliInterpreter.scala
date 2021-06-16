package ltbs.uniform
package interpreters.cli

import validation.Rule
import izumi.reflect.Tag

trait ConsoleAsk[A] {

  def name: String
  def read(in: String): Either[String, A]
  def apply(
    key: String,
    default: Option[A],
    validation: Rule[A]
  ): Either[String,A] =
    read(scala.io.StdIn.readLine(s"$key [$name]: "))
}

object ConsoleAsk {
  def apply[A: Tag](f: String => Either[String,A]) = new ConsoleAsk[A] {
    def read(in: String) = f(in)
    def name: String = implicitly[Tag[A]].tag.shortName
  }
}

trait ConsoleTell[A] {

  def format(in: A): String

  def apply(
    key: String,
    value: A
  ): Unit = { println(s"$key : ${format(value)}") }
}

object ConsoleTell {
  implicit def anyInstance[A] = new ConsoleTell[A] {
    def format(in: A): String = in.toString
  }
}

case class ConsoleInterpreter() extends MonadInterpreter[Either[String,+?], ConsoleAsk, ConsoleTell, ConsoleAsk] {

  def monadInstance = implicitly[cats.Monad[Either[String,+?]]]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    asker: ConsoleAsk[A]
  ): Either[String, A] = asker(key, default, validation)

  override def tellImpl[T](
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])],
    teller: ConsoleTell[T]
  ): Either[String,Unit] = Right(teller(key, value))

  override def endImpl(
    key: String,
    customContent: Map[String,(String,List[Any])]
  ): Either[String, Nothing] = Left(s"End of journey: $key")

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => Either[String,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String,List[Any])],
    asker: ConsoleAsk[A]
  ): Either[String,List[A]] = ???
}
