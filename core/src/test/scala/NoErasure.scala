package ltbs.uniform

import org.scalatest._
import cats._, implicits._
import scala.language.higherKinds
import shapeless.{the ⇒ _, _}, ops.hlist.Selector

class NoErasure extends FlatSpec with Matchers {

  "A interpreter" should "not suffer type erasure" in {
    trait TC[A] { val i: A }

    implicit val stringInst = new TC[String] { val i = "test" }
    implicit val booleanInst = new TC[Boolean] { val i = false }

    val i = TypeclassList[String :: Boolean :: HNil, TC]
    i.forType[String].i shouldBe ("test")

    val t = shapeless.the[TypeclassList[String :: HNil, TC]]
    t.forType[String].i shouldBe ("test")
  }

}
