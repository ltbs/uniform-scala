package ltbs.uniform.gformsparser

import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {
  import Parser._

  val four = 4

  "gformExpr" should "do sums" in {
    """val i: Int = gformExpr("four+1")""" should compile
    gformExpr("four+1") shouldBe 5
    gformExpr("four/2") shouldBe 2
    gformExpr("four-1") shouldBe 3
  }

  it should "do conditional tests" in {    
    """gformExpr("four")""" shouldNot compile
    """val i: Boolean = gformExpr("four=4")""" should compile
    gformExpr("four=4") shouldBe true
    gformExpr("four=5") shouldBe false
  }

}
