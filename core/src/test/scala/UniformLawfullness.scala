package ltbs.uniform

import cats.implicits._

// import cats.laws.discipline._
// import munit.DisciplineSuite
// import cats.implicits._
// import cats.laws.discipline.MonadTests
// import org.scalacheck._
// import cats.laws.discipline.SemigroupalTests.Isomorphisms

// doesn't work in uninterpretted form -
// 
// x.some.map(identity) == x.some
// Uf.Pure(x).map(identity) == Uf.Map(Uf.Pure(x), identity) != Uf.Pure(x)

// class UniformLawfullness extends DisciplineSuite {

//   implicit val iso: Isomorphisms[Uniform[Needs.Ask[Int], *, Unit]] =
//     Isomorphisms.invariant[Uniform[Needs.Ask[Int], *, Unit]]


//   implicit def eqUniform[R <: Needs[_], A]: cats.Eq[Uniform[R, A, Unit]] = new cats.Eq[Uniform[R, A, Unit]] {
//     def eqv(x: Uniform[R,A,Unit], y: Uniform[R,A,Unit]): Boolean = x == y
//   }

//   implicit def arbitraryUniform[R <: Needs[_], A](implicit arb: Arbitrary[A]): Arbitrary[Uniform[R, A, Unit]] =
//     Arbitrary(arb.arbitrary.map(x => pure(x)))

//   checkAll("Uniform.FunctorLaws", MonadTests[Uniform[Needs.Ask[Int], ?, Unit]].monad[Int, Int, Int])
// }

class UniformSuite extends munit.FunSuite {

  val forComp: Uniform[Needs.Ask[Int],Unit,(Int, Int)] = for {
    a <- ask[Int]("a")
    b <- ask[Int]("b")    
  } yield (a,b)

  // implicit def uniformFunctor: cats.Functor[Uniform[Needs.Ask[Int],Int,*]] = ???
  // implicit def uniformSemigroupal: cats.Semigroupal[Uniform[Needs.Ask[Int],Int,*]] = ???  

  implicit def uniformFunctor: cats.Functor[Uniform[Needs.Ask[Int],*,Unit]] = ???
  implicit def uniformSemigroupal: cats.Semigroupal[Uniform[Needs.Ask[Int],*,Unit]] = ???  

  val forComp2: Uniform[Needs.Ask[Int],Unit,(Int, Int)] = (
    ask[Int]("a"),
    ask[Int]("b")    
  ).tupled

  
}
