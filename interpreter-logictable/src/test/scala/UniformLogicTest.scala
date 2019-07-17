package ltbs.uniform
package interpreters.logictable

import cats.implicits._
import examples.beardtax._

object UniformLogicTest {

  implicit val optMopSamples = new SampleData[Option[MemberOfPublic]] {
    def apply(key: String):List[Option[MemberOfPublic]] = List(
      Some(MemberOfPublic(NonEmptyString("john"), NonEmptyString("johnson"), java.time.LocalDate.now))
    )
  }

  implicit val beardStyleSamples = new SampleData[BeardStyle] {
    def apply(key: String):List[BeardStyle] = List(
      BeardStyle.Gunslinger(1),
      BeardStyle.LaughingCavalier
    )
  }

  implicit val beardLengthSamples = new SampleData[BeardLength] {
    def apply(key: String):List[BeardLength] = List(
      (1,2), (-1,3)
    )
  }

  val interpreter = new LogicTableInterpreter[TellTypes, AskTypes]()

  val dummyHod = new Hod[Logic] {
    def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Logic[Int] =
      IdDummyHod.costOfBeard(beardStyle, length).pure[Logic]
  }

  def main(args: Array[String]): Unit = {
    val output: List[(List[String], Either[ErrorTree,Int])] = beardProgram(
      interpreter, dummyHod
    ).value.run

    output.foreach{ case (messages, outcome) ⇒
      println(messages.mkString("\n"))
      println(s"   ⇒ $outcome")
    }
  }

}
