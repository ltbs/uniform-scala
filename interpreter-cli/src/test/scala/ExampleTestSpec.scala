package ltbs.uniform.interpreters.cli

import ltbs.uniform._
import cats.implicits._
import org.atnos.eff._, all.{ask => _}, syntax.all._

object ExampleTestApp extends App {

  def program[R
      : _uniformCore
      : _uniformAsk[Int,?]
  ]: Eff[R, Int] =
    ask[Int]("single").in[R]

  val out = program[Fx.fx3[cats.data.State[UniformCore,?], UniformAsk[Int,?], cats.Eval]]
    .using(_.toInt)
    .runEval
    .runState(UniformCore())
    .run

  println(out)
}
