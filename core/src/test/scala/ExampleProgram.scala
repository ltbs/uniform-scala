package ltbs.uniform

import cats._, implicits._
import com.github.ghik.silencer.silent

case class SillyEmpty[OUT]()(implicit m: Monoid[OUT]) {
  def empty: OUT = m.empty
}

class SyntaxSpec extends munit.FunSuite {

  test("A program should be compilable and executable with asks, tells and interacts") {
    val program = {
      for {
        a <- interact[Int]("hiya", "in")
        b <- interact[Int]("hiya2", "in")
        c <- ask[Option[String]]("c")
        _ <- tell[Option[String]]("_", c)
      } yield ((a + b, c))
    }
  }

}
