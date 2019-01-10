package ltbs.uniform.web

import org.scalatest._
import ltbs.uniform._

class FormUrlEncodedSpec extends FlatSpec with Matchers {

  val sample: FormUrlEncoded = Map(
    "one" -> List("val1", "val2"),
    "one.child" -> List("valc1"),
    "one.child.grandchild" -> List("valg1"),
    "one.childb" -> List("valc1")
  )

  def oneLine(in: String): String =
    in.stripMargin.lines.mkString("&")

  val sampleString = oneLine(
    """|one[]=val1
       |one[]=val2
       |one.child=valc1
       |one.child.grandchild=valg1
       |one.childb=valc1"""
  )

  val sampleTree: Input = Tree(Nil, Map(
    "one" -> Tree(List("val1", "val2"),
      Map(
        "child" -> Tree(List("valc1"),
          Map(
            "grandchild" -> Tree(List("valg1"), Map.empty
            ))
        ),
        "childb" -> Tree(List("valc1"), Map.empty)
        )
    ))
  )

  "FormUrlEncoded" should "be writeable to a string" in {
    sample.writeString should be (sampleString)
  }

  it should "be readable from a string" in {
    FormUrlEncoded.readString(sampleString) should be (sample)
  }

  it should "be writeable to an InputTree" in {
    sample.toInputTree should be (sampleTree)
  }

  it should "be readable from an InputTree" in {
    FormUrlEncoded.fromInputTree(sampleTree) should be (sample)
  }

  it should "be navigable to a child element" in {
    sample.forestAtPath("one", "child") should be (
      FormUrlEncoded.readString(oneLine(
      """|=valc1
         |grandchild=valg1"""
    )))
  }
}
