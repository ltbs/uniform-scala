package ltbs.uniform
package interpreters.logictable

import java.time.LocalDate

import ltbs.uniform.validation.Rule

object UniformLogicTest {

  val journey = for {
    a <- ask[String]("one")
    b <- ask[Boolean]("two", validation = Rule.alwaysPass)
    c <- ask[LocalDate]("three")
    _ <- end("four", (a,b,c))
  } yield (a,b, c)

  implicit val stringSample = SampleData.instance("foo")

  implicit val booleanSample = SampleData.instance(true)

  implicit val dateSample = SampleData.instance(LocalDate.now)

  def main(args: Array[String]): Unit = {
    val output = LogicTableInterpreter.execute(journey)
    
    output match {
      case Left(x) => println(s"journey left output: $x")
      case Right(y) => println(s"journey right output: $y")
    }

  }

}
