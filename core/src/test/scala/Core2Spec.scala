package ltbs.uniform

import cats.{Id, Monoid}
import shapeless.HList
import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers

trait Noop[A] {}

// object MonoidInterpreter2 extends Interpreter[Id, Monoid, Noop] {

//   def executeImpl[H <: Needs[_], A, AskTypes <: HList, TellTypes <: HList](
//     program: Uniform[H, A], 
//     askKlist: TypeclassList[AskTypes, Monoid],
//     tellKlist: TypeclassList[TellTypes, Noop],
//   ): A = ???

// }

// class TestInterpreter2 extends AnyFlatSpec with Matchers {

//   "A simple interpreter" should "be able to compose Id monad instances" in {

//     val journey: Uniform[Needs.Ask[Int] with Needs.Tell[String] with Needs.Ask[Option[String]] with Needs.Tell[Option[String]],(Int, Option[String])] = for {
//       a <- interact[Int,String]("hiya", "in")
//       b <- interact[Int,String]("hiya2", "in")
//       c <- ask[Option[String]]("c")
//       _ <- tell[Option[String]]("_", c)
//     } yield ((a + b, c))

// //    MonoidInterpreter2.execute[Needs.Ask[Int] with Needs.Tell[String] with Needs.Ask[Option[String]] with Needs.Tell[Option[String]], (Int, Option[String])](journey)


//   }
// }
