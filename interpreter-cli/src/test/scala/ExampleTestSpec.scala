package ltbs.uniform.interpreters.cli

import ltbs.uniform._
import cats.implicits._
import org.atnos.eff._, all._, syntax.all._

object ExampleTestApp extends App {

  type UFInt[A] = _uniformAsk[Int,A]

  def program[R](implicit rint: UniformAsk[Int,?] |= R): Eff[R, Int] = 
    uask[Int, R]("single")

  val out = program[Fx.fx2[UniformAsk[Int,?], cats.Eval]]
    .using(_.toInt)
    .runEval
    .run

  println(out)
}
