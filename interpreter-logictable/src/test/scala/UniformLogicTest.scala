package ltbs.uniform

import org.atnos.eff._
import cats.implicits._
import org.atnos.eff.syntax.all._
import UniformTest._
import LogicTableInterpreter._

object UniformLogicTest {

  val output = program[FxAppend[TestProgramStack, LogicTableStack]]
    .giveExamples(List(true,false))
    .giveExamples(
      for {
        lower <- (0L to 4L).toList.map(_ * 500000)
        higher <- (0L to 4L).toList.map(_ * 500000)
      } yield ((lower,higher)))
    .runEither
    .runWriter
    .runList
    .run

  def main(args: Array[String]): Unit = {
  output.foreach{ case (a,b) =>
    println(s"""${b.mkString(",")} => ${a.toValidated}""")
  }
  }

}
