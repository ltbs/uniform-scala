package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import scala.language.higherKinds

class ColdImplicitSpec extends AnyFlatSpec with Matchers {
  import scala.language.experimental.macros
  def coldImplicit[A, TC[_]]: TC[A] = macro InterpreterMacros.coldImplicit[A, TC]

  "coldImplicit" should "work for simple higher kinded types" in {
    implicit val i: Option[String] = Some("test")
    coldImplicit[String, Option] should be (i)
  }

  it should "work for kind projected types" in {
    implicit val i: Either[Int, String] = Right("test")
    coldImplicit[Int, Either[?, String]] should be (i) 
  }

  it should "work for type aliases" in {
    type EitherInt[A] = Either[Int, A]
    implicit val i: Either[Int, String] = Right("test")
    coldImplicit[String, EitherInt] should be (i) 
  }


}
