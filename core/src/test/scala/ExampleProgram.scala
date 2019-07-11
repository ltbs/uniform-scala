package ltbs.uniform

import org.scalatest._
import cats._, implicits._
import scala.language.higherKinds
import com.github.ghik.silencer.silent

case class SillyEmpty[OUT]()(implicit m: Monoid[OUT]) {
  def empty: OUT = m.empty
}

class SyntaxSpec extends FlatSpec with Matchers {

  "A program" should "be compilable and executable with asks, tells and interacts" in {

    // the data types that the user can be presented with in a `tell'
    type TellTypes = String :: Option[String] :: NilTypes

    // the data types that the user can be prompted for in an `ask'
    type AskTypes = Int :: Option[String] :: NilTypes

    @silent def program[F[_]: Monad](
      interpreter: Language[F, TellTypes, AskTypes]
    ): F[(Int, Option[String])] = {
      import interpreter._
      for {
        a ← interact[String,Int]("hiya", "in")
        b ← interact[String,Int]("hiya2", "in")
        c ← ask[Option[String]]("c")
        _ ← tell[Option[String]]("_", c)
      } yield ((a + b, c))
    }
  }

}
