package ltbs.uniform
package interpreters.cli

import cats.implicits._
import reflect.runtime.universe.WeakTypeTag
import shapeless._, ops.hlist.Selector
import language.higherKinds

trait TellCli[A] {
  def render(in: A): String
}

trait AskCli[A] {
  def apply(in: String, validation: List[List[Rule[A]]]): A
}

class CliInterpreter[
  SupportedTell <: HList : Summoner[?, TellCli],
  SupportedAsk  <: HList : Summoner[?, AskCli]
] extends Language[Id, SupportedTell, SupportedAsk] {

  override def interact[Tell: WeakTypeTag, Ask: WeakTypeTag](
    id: String,
    t: Tell,
    default: Option[Ask],
    validation: List[List[Rule[Ask]]],
    customContent: Map[String,(String,List[Any])]
  )(
    implicit selectorTell : Selector[SupportedTell, Tell],
    selectorAsk : Selector[SupportedAsk, Ask]
  ): Ask = {
    val teller = the[Summoner[SupportedTell,TellCli]].instanceFor[Tell].render(t)
    val asker = the[Summoner[SupportedAsk,AskCli]].instanceFor[Ask]
    print(teller)
    asker(id, validation)
  }
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
      one <- ask[Int]("one")
      two <- ask[Int]("two")
      _ <- interact[String, Boolean]("confirm", tell = s"Confirm you are happy with ${one + two}")
    } yield (one + two)
  }

  implicit def tellAny[A](implicit lp:LowPriority): TellCli[A] = new TellCli[A] {
    def render(in: A): String = in.toString + "\n"
  }

  implicit val tellUnit = new TellCli[Unit] {
    def render(in: Unit): String = ""
  }

  def askCliInstance[A](f: String => Either[String,A]) = new AskCli[A] {
    @annotation.tailrec
    def apply(key: String, validation: List[List[Rule[A]]]): A = {
      print(s"$key: ")
      val rawIn = io.StdIn.readLine()

      f(rawIn) match {
        case Left(err) => println(err); apply(key, validation)
        case Right(v) => v
      }
    }
  }

  implicit val askString = askCliInstance[String](x => Right(x))
  implicit val askInt = askCliInstance(x => util.Try(x.toInt)
    .toEither
    .leftMap(_.getLocalizedMessage())
  )

  implicit val askBool = askCliInstance{
    case "Y" => Right(true)
    case "N" => Right(false)
    case _ => Left("please enter 'Y' or 'N'")
  }

  implicit val askUnit = askCliInstance { _ => Right(()) } 

  program(new CliInterpreter)
}
