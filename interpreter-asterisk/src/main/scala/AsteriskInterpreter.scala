package ltbs.uniform
package interpreter.asterisk

import language.higherKinds

import cats.effect._
import cats.data.ReaderT
import cats.implicits._
import org.asteriskjava.fastagi._
import validation.Rule

trait TellAsterisk[A] {
  def apply(key: String, in: A): ReaderT[IO, AgiOperations, Unit]
}

object TellAsterisk {
  def instance[A](f: (String,A,AgiOperations) => Unit): TellAsterisk[A] =
    new TellAsterisk[A] {
      def apply(key: String, in: A): ReaderT[IO, AgiOperations, Unit] = for {
        ops <- ReaderT.ask[IO, AgiOperations]
        _   <- ReaderT.liftF(IO { f(key, in, ops) } )
      } yield ()
    }
}

trait AskAsterisk[A] {
  def apply(in: String, validation: Rule[A]): ReaderT[IO, AgiOperations, A]
}

object AskAsterisk {
  def instance[A](msg:String, f: AgiOperations => A): AskAsterisk[A] =
    new AskAsterisk[A] {
      def apply(key: String, validation: Rule[A]): ReaderT[IO, AgiOperations, A] = for {
        ops <- ReaderT.ask[IO, AgiOperations]
        _   <- ReaderT.liftF(IO { ops.exec("Flite", key) } )
        _   <- ReaderT.liftF(IO { ops.exec("Flite", msg) } )
        ret <- ReaderT.liftF(IO { f(ops) } )
      } yield ret
    }
}

case class InteractAsterisk[T,A](tell: TellAsterisk[T], ask: AskAsterisk[A])

object InteractAsterisk {
  implicit def auto[T,A](
    implicit tell: TellAsterisk[T],
    ask: AskAsterisk[A]
  ): InteractAsterisk[T,A] = InteractAsterisk(tell, ask)

  def asteriskOp[A](f: AgiOperations => A): ReaderT[IO, AgiOperations, A] = for {
    ops <- ReaderT.ask[IO, AgiOperations]
    r   <- ReaderT.liftF(IO { f(ops) } )
  } yield r
}

trait AsteriskInterpreter extends RecoverableMonadInterpreter[
  ReaderT[IO, AgiOperations, *],
  InteractAsterisk,
  Noop /* TODO Listings */
] {

  def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => ReaderT[IO, AgiOperations, A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
    asker: Noop[A]
  ): ReaderT[IO, AgiOperations, List[A]] = ???

  def errorNotification(error: ErrorTree): ReaderT[IO, AgiOperations, Unit] =
    error match {
      case ErrorTree.empty => ().pure[ReaderT[IO, AgiOperations, *]]
      case _ => InteractAsterisk.asteriskOp(_.exec("Flite", "invalid input"))
    }

  def interactRecoverable[T, A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String, List[Any])],
    interaction: InteractAsterisk[T,A],
    error: ErrorTree
  ): ReaderT[IO, AgiOperations, A] = 
    errorNotification(error) >> 
    interaction.tell(key, tellValue) >>
    interaction.ask(key, validation)


  def monadInstance: cats.Monad[ReaderT[IO, AgiOperations, *]] = implicitly

}
