package ltbs.uniform

import org.scalatest._
import ltbs.uniform._

class TreeSpec extends FlatSpec with Matchers {

  val in: FormUrlEncoded = Map("one.two.three" -> Seq("findme"))

  "Trees" should "be addressable by path" in {
    in.toInputTree.atPath("one", "two", "three") should be (Some(Seq("findme")))
  }

  it should "be able to find a subforest" in {
    val expected: Input =
      Tree(List(),Map("two" -> Tree(List(),Map("three" -> Tree(List("findme"),Map())))))
    in.toInputTree.forestAtPath("one") should be (Some(expected))
  }

}
