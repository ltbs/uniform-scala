package ltbs.uniform
package interpreters

import cats.implicits._
import shapeless._
import com.github.ghik.silencer.silent

package object cli {
  implicit val tellUnit = new TellCli[Unit] {
    def render(in: Unit): String = ""
  }

  implicit def tellAny[A](
    implicit @silent lp:LowPriority
  ): TellCli[A] = new TellCli[A] {
    def render(in: A): String = in.toString + "\n"
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
    case _   => Left("please enter 'Y' or 'N'")
  }

  implicit val askUnit = askCliInstance { _ => Right(()) }

}
