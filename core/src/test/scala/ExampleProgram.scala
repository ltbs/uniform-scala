package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import cats._, implicits._
import com.github.ghik.silencer.silent

case class SillyEmpty[OUT]()(implicit m: Monoid[OUT]) {
  def empty: OUT = m.empty
}

class SyntaxSpec extends AnyFlatSpec with Matchers {

  "A program" should "be compilable and executable with asks, tells and interacts" in {

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
