package ltbs.uniform

import org.scalatest._
import org.atnos.eff._, all._, syntax.all._
import cats._, implicits._

case class SillyEmpty[IN,OUT]()(implicit m: Monoid[OUT]) {
  def empty: OUT = m.empty
}

class SyntaxSpec extends FlatSpec with Matchers {

  "A program" should "be compilable and executable with asks, tells and interacts" in {

    def program[R : _uniform[String,Int,?] : _uniformAsk[Option[String],?] : _uniformTell[Option[String],?]]: Eff[R,(Int, Option[String])] = for {
      a <- uniform[String,Int,R]("hiya","in")
      b <- uniform[String,Int,R]("hiya2","in")
      c <- uask[Option[String],R]("c")
      _ <- utell[Option[String],R]("_",c)
    } yield ((a + b,c))

    implicit class ZeroOps[R, A](e: Eff[R, A]) {
      def fromMonoid[IN,OUT, U](
        sillyEmpty: SillyEmpty[IN,OUT]
      )(
        implicit member: Member.Aux[Uniform[IN,OUT,?], R, U],
        evalM:_eval[U]
      ): Eff[U, A] =
        e.translate(
          new Translate[Uniform[IN,OUT,?], U] {
            def apply[X](ax: Uniform[IN,OUT,X]): Eff[U, X] =
              send(
                Eval.later{
                  sillyEmpty.empty.asInstanceOf[X]
                }
              )
          })
    }

    type STACK = Fx.fx4[
      Uniform[String,Int,?],
      UniformTell[Option[String],?],
      UniformAsk[Option[String],?],
      Eval
    ]

    val output = program[STACK]
      .fromMonoid(SillyEmpty[String,Int])
      .fromMonoid(SillyEmpty[Unit,Option[String]])
      .fromMonoid(SillyEmpty[Option[String],Unit])
      .runEval
      .run
    output should be ((0,None))
  }

}
