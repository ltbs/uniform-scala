package ltbs.uniform

import scala.language.higherKinds
import scala.language.experimental.macros

class ColdImplicitSpec extends munit.FunSuite {

  def coldImplicit[A, TC[_]]: TC[A] = macro InterpreterMacros.coldImplicit[A, TC]

  test("coldImplicit") {
    test("simple higher kinded types") {
      implicit val i: Option[String] = Some("test")
      assertEquals(coldImplicit[String, Option], i)
    }

    test("kind projected types") {
      implicit val i: Either[Int, String] = Right("test")
      assertEquals(coldImplicit[Int, Either[?, String]], i)
    }

    test("type aliases") {
      type EitherInt[A] = Either[Int, A]
      implicit val i: Either[Int, String] = Right("test")
      assertEquals(coldImplicit[String, EitherInt], i)
    }

  }
}
