package ltbs.uniform.interpreters.cli

import ltbs.uniform._
import cats.implicits._
import org.atnos.eff._, all._, syntax.all._

object ExampleTestApp extends App {

  type UFInt[A] = _uniform[Int,A]
  type UFIntList[A] = _uniformList[Int,A]

  def program[R
      : UFInt
      : UFIntList
  ]: Eff[R, (Int,List[Int])] = (
    uask[R, Int]("single"),
    uaskList[R, Int]("list")
  ).tupled

  val out = program[Fx.fx3[UniformAsk[Int,?], UniformAskList[Int,?], cats.Eval]]
    .using(_.toInt)
    .usingList(_.toInt)
    .runEval
    .run

  println(out)
}
