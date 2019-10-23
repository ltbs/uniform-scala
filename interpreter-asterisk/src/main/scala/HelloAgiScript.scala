package ltbs.uniform
package interpreter.asterisk

import language.higherKinds

import cats.effect._
import cats.implicits._
//import com.github.ghik.silencer.silent
import org.asteriskjava.fastagi._
import shapeless._

trait TellAsterisk[A] {
  def apply(key: String, in: A): IO[Unit]
}

trait AskAsterisk[A] {
  def apply(in: String, validation: List[List[Rule[A]]]): IO[A]
}

class AsteriskInterpreter[
  SupportedTell <: HList : TypeclassList[?, TellAsterisk],
  SupportedAsk  <: HList : TypeclassList[?, AskAsterisk]  
] extends Language[IO, SupportedTell, SupportedAsk] {

  override def interact[Tell, Ask](
    id            : String,
    t             : Tell,
    default       : Option[Ask],
    validation    : List[List[Rule[Ask]]],
    customContent : Map[String,(String,List[Any])]
  )( implicit
    selectorTell : IndexOf[SupportedTell, Tell],
    selectorAsk  : IndexOf[SupportedAsk, Ask]
  ): IO[Ask] = {
    val teller = the[TypeclassList[SupportedTell,TellAsterisk]].
      forType[Tell].apply(id, t)
    val asker = the[TypeclassList[SupportedAsk,AskAsterisk]].
      forType[Ask]
    teller >> asker(id, validation)
  }  

}

object AgiServer extends BaseAgiScript with App {

  type TellTypes = Int :: NilTypes
  type AskTypes = Int :: NilTypes

  case class Submission(x: Int, y: Int)

  def program[F[_] : cats.Monad](interpreter: Language[F, TellTypes, AskTypes]): F[Submission] = {
    import interpreter._

    for {
      x <- ask[Int]("first number")
      y <- ask[Int]("second number")
      _ <- tell("the result", x * y)
    } yield Submission(x,y)
  }

  implicit val askUnit = new AskAsterisk[Unit] {
    def apply(in: String, validation: List[List[Rule[Unit]]]): IO[Unit] = ().pure[IO]
  }

  implicit val askInt = new AskAsterisk[Int] {
    def apply(in: String, validation: List[List[Rule[Int]]]): IO[Int] = IO {
      exec("Flite", in);
      exec("Flite", "please enter a number followed by the hash sign");
      getData("silence/1",20000).toInt
    }
  }

  implicit val tellUnit = new TellAsterisk[Unit] {
    def apply(key: String, in: Unit): cats.effect.IO[Unit] = ().pure[IO]
  }


  implicit val tellInt = new TellAsterisk[Int] {
    def apply(key: String, in: Int): cats.effect.IO[Unit] = IO {
      exec("Flite", s"$key is $in");
      ()
    }
  }

  def service(request: AgiRequest, channel: AgiChannel): Unit = {
    answer();
    streamFile("silence/1")
    val out: Submission = program(new AsteriskInterpreter[TellTypes, AskTypes]).unsafeRunSync()
    println(out)
    hangup();
  }

  val server = new DefaultAgiServer(this)
  server.startup()
}
