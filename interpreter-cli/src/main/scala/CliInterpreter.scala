package ltbs.uniform
package interpreters.cli

import cats.implicits._
import shapeless._
import language.higherKinds
import com.github.ghik.silencer.silent

trait TellCli[A] {
  def render(in: A): String
}

trait AskCli[A] {
  def apply(in: String, validation: List[List[Rule[A]]]): A
}

class CliInterpreter[
  SupportedTell <: HList : TypeclassList[?, TellCli],
  SupportedAsk  <: HList : TypeclassList[?, AskCli]
] extends Language[Id, SupportedTell, SupportedAsk] {

  override def interact[Tell, Ask](
    id            : String,
    t             : Tell,
    default       : Option[Ask],
    validation    : List[List[Rule[Ask]]],
    customContent : Map[String,(String,List[Any])]
  )( implicit
    selectorTell : IndexOf[SupportedTell, Tell],
    selectorAsk  : IndexOf[SupportedAsk, Ask]
  ): Ask = {
    val teller = the[TypeclassList[SupportedTell,TellCli]].
      forType[Tell].render(t)
    val asker = the[TypeclassList[SupportedAsk,AskCli]].
      forType[Ask]
    print(teller)
    asker(id, validation)
  }
}

object CliInterpreter {

  implicit val tellUnit = new TellCli[Unit] {
    def render(in: Unit): String = ""
  }

  implicit def tellAny[A](
    implicit @silent lp:LowPriority
  ): TellCli[A] = new TellCli[A] {
    def render(in: A): String = in.toString + "\n"
  }

  def askCliInstance[A](f: String ⇒ Either[String,A]) = new AskCli[A] {
    @annotation.tailrec
    def apply(key: String, validation: List[List[Rule[A]]]): A = {
      print(s"$key: ")
      val rawIn = io.StdIn.readLine()

      f(rawIn) match {
        case Left(err) ⇒ println(err); apply(key, validation)
        case Right(v) ⇒ v
      }
    }
  }

  implicit val askString = askCliInstance[String](x ⇒ Right(x))
  implicit val askInt = askCliInstance(x ⇒ util.Try(x.toInt)
    .toEither
    .leftMap(_.getLocalizedMessage())
  )

  implicit val askBool = askCliInstance{
    case "Y" ⇒ Right(true)
    case "N" ⇒ Right(false)
    case _   ⇒ Left("please enter 'Y' or 'N'")
  }

  implicit val askUnit = askCliInstance { _ ⇒ Right(()) }

}

object CliApp extends App {

  type TellTypes = Unit :: String :: HNil
  type AskTypes = Unit :: Int :: String :: Boolean :: HNil

  def program[F[_]: cats.Monad](interpreter: Language[F,  // F must be a monad
    TellTypes,
    AskTypes // data types we're interested in
  ]): F[Int] = {
    import interpreter._

    for {
      one ← ask[Int]("one")
      two ← ask[Int]("two")
      _   ← interact[String, Boolean]("confirm", tell = s"Confirm you are happy with ${one + two}")
    } yield (one + two)
  }

  import CliInterpreter._
  program(new CliInterpreter[TellTypes, AskTypes])
}
