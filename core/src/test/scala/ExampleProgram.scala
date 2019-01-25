package ltbs.uniform

import org.scalatest._
import org.atnos.eff._

class SyntaxSpec extends FlatSpec with Matchers {

  "A program" should "be compilable with lists and single values" in {

    """|type UFInt[A] = _uniform[Int,A]
       |type UFIntList[A] = _uniformList[Int,A]
       | 
       |def program[R
       |  : UFInt
       |  : UFIntList
       |]: Eff[R, (Int,List[Int])] =
       |  for {
       |    single <- uask[R, Int]("single")
       |    plural <- uaskList[R, Int]("list")
       |  } yield (single,plural)""".stripMargin should compile
    
  }

}
