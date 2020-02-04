package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers

class NoErasure extends AnyFlatSpec with Matchers {

  "A TypeclassList" should "not suffer type erasure" in {
    trait TC[A] { val i: A }

    implicit val stringInst = new TC[String] { val i = "test" }
    implicit val booleanInst = new TC[Boolean] { val i = false }

    val i = TypeclassList[String :: Boolean :: shapeless.HNil, TC]
    i.forType[String].i shouldBe ("test")

  }

}
