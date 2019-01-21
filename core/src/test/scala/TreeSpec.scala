package ltbs.uniform

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  val in: Tree[String,List[String]] = Tree(List(),
    Map("one" -> Tree(List("oneval"),
      Map("two" -> Tree(List(),
        Map("three" -> Tree(
          List("findme"),Map())))))))

  "Trees" should "be addressable by path" in {
    in.atPath("one", "two", "three") should be (Some(Seq("findme")))
  }

  it should "be able to find a subforest" in {
    val expected: Tree[String,List[String]] =
      Tree(List("oneval"),Map("two" -> Tree(List(),Map("three" -> Tree(List("findme"),Map())))))
    in.forestAtPath("one") should be (Some(expected))
  }

}
