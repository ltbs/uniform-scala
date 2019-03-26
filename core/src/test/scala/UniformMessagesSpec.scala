package ltbs.uniform

import cats.Eq
import cats.kernel.laws.discipline._
import cats.laws.discipline._
import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}

class UniformMessageSpec extends cats.tests.CatsSuite {

  implicit def arbFoo[A: Arbitrary]: Arbitrary[UniformMessages[A]] = {
    val innerMap = for {
      v <- Gen.listOf(Arbitrary.arbitrary[A]).map(_.map(List(_)))
    } yield (UniformMessages.fromMap({v.zipWithIndex.map{case (v,i) => (i.toString,v)}}.toMap))
    Arbitrary(innerMap)
  }

  implicit def eqMessages[A: Eq] = new Eq[UniformMessages[A]]{
    def eqv(x: UniformMessages[A], y: UniformMessages[A]): Boolean = {

      @annotation.tailrec
      def inner(count: Int): Boolean = {
        val i = (x.get(count.toString), y.get(count.toString))
        i match {
          case (Some(xv),Some(yv)) if xv === yv => inner(count + 1)
          case (None, None) => true
          case _ => false
        }
      }
      inner(0)
    }
  }

  checkAll("Functor[UniformMessages]", FunctorTests[UniformMessages].functor[Int, Int, String])
  checkAll("Monoid[UniformMessages]", MonoidTests[UniformMessages[String]].monoid)

  test("noop |+| echo should never give an empty value") {
    val messages = UniformMessages.noop[String] |+| UniformMessages.echo
    messages("hiya") should be ("hiya")
  }

}
