package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers

class NoErasure extends AnyFlatSpec with Matchers {

  trait TC[A] { val i: A }

  implicit val stringInst = new TC[String] { val i = "test" }
  implicit val booleanInst = new TC[Boolean] { val i = false }

  "A TypeclassList" should "not suffer type erasure" in {
    val i = TypeclassList[String :: Boolean :: shapeless.HNil, TC]
    i.forType[String].i shouldBe ("test")
  }

  it should "function when using macros" in {
    val i = TypeclassList[String :: Boolean :: shapeless.HNil, TC]
    type FU = String :: Boolean :: shapeless.HNil
    val v = implicitly[TypeclassList[FU, TC]]
    i.forType[String].i shouldBe ("test")
    v.forType[String].i shouldBe ("test")    
  }
}
