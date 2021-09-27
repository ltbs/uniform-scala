package ltbs.uniform
package interpreter.asterisk
package test

import language.higherKinds

import cats.effect._
import cats.data.ReaderT
import cats.implicits._
import org.asteriskjava.fastagi._
import validation.Rule

case class Submission(x: Int, y: Int)

object AgiServer extends BaseAgiScript with App with AsteriskInterpreter {

  def journey = for {
    _ <- tell("welcome to the multiplication server", ())    
    x <- ask[Int]("first number")
    y <- ask[Int]("second number")
    _ <- tell("the result", x * y)
  } yield Submission(x,y)

  implicit val askInt: AskAsterisk[Int] = AskAsterisk.instance(
    "please enter a number followed by the hash sign",
    _.getData("silence/1",20000).toInt
  )

  implicit val askUnit: AskAsterisk[Unit] = AskAsterisk.instance(
    "please press any key to continue",
    {ops => ops.getData("silence/1",20000,1); ()}
  )

  implicit val tellInt: TellAsterisk[Int] = TellAsterisk.instance{
    case (key, in, ops) => ops.exec("Flite", s"$key is $in")
  }

  implicit val tellUnit: TellAsterisk[Unit] = TellAsterisk.instance{
    case _ => ()
  }

  def service(request: AgiRequest, channel: AgiChannel): Unit = {
    answer();
    streamFile("silence/1")
    interpret(journey).run(this).unsafeRunAsync {
      case Left(err) =>
        hangup()
        throw err
      case Right(submission) =>
        println(submission)
        hangup()
    }
    
  }

  val server = new DefaultAgiServer(this)
  server.startup()
}
