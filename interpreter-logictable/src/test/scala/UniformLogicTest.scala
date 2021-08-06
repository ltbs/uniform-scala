package ltbs.uniform
package interpreters.logictable

import java.time.LocalDate

import ltbs.uniform.validation.Rule

object UniformLogicTest {

  val journey = for {
    a <- ask[String]("one")
    b <- ask[Boolean]("two", validation = Rule.cond(identity, "false not accepted"))
    c <- ask[LocalDate]("three")
    dx <- askList[LocalDate]("mylist") { case _ => ask[LocalDate]("myited") }
  } yield (a,b, c, dx)


  implicit val stringSample = SampleData.instance("foo", "bar")

  implicit val booleanSample = SampleData.instance(true, false)

  implicit val dateSample = SampleData.instance(LocalDate.now)

  implicit def anythingRenderer[A]: TellRenderer[A] = ???

  implicit val nothingSample = new SampleData[Nothing] {
    def apply(key: String): List[Nothing] = Nil
  }

  implicit val dateSamplieSize = SampleListQty[LocalDate](3)

  def main(args: Array[String]): Unit = {
    val output = LogicTableInterpreter.interpret(journey).value.value
    
    output.foreach {
      case Left(x) => println(s"journey left output: $x")
      case Right(y) => println(s"journey right output: $y")
    }
    
  }

}
