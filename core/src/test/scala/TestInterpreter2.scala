package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import cats.implicits._
import cats.{Id, Monad}
import shapeless.{Id => _, _}
import scala.language.higherKinds
import validation.Rule
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

trait Example[A] { val value: A }

object ListInterpreter extends Interpreter[List, Example, Noop] {


  def ask[A](key: String, asker: Example[A]): List[A] = List(asker.value)
  def tell[A](key: String, value: A, teller: Noop[A]): List[Unit] = Nil


  def executeImpl[H <: Needs[_], A: Tag](
    program: Uniform[H,A],
    askMap: Map[LightTypeTag, Example[_]],    
    tellMap: Map[LightTypeTag, Noop[_]]
  ): List[A] = {
    import ltbs.uniform.{Uniform => U}
    program match {
      case U.Map(base, f) => executeImpl(base, askMap, tellMap).map(f)
      case U.FlatMap(base, f) => executeImpl(base, askMap, tellMap).flatMap(f.map(executeImpl(_, askMap, tellMap)))
      case U.Tell(key, value, tag) => Nil // tell(key, value, tellMap(tag.tag))
      case U.Interact(key, value, askTag, tellTag) => Nil
      case U.Ask(key, tag) => ask(key, askMap(tag.tag))
      case U.EndTell(_, _, _) => Nil
      case U.End(_) => Nil
      case U.Pure(v) => List(v)
    }
  }
  
}

class TestInterpreter3 extends AnyFlatSpec with Matchers {

  def example[A](x: A): Example[A] = new Example[A] { val value: A = x }
  implicit def noop[A]: Noop[A] = new Noop[A] {}

  "A simple interpreter" should "be able to compose Id monad instances" in {

    implicit val exInt = example[Int](1)
    implicit val exOptString = example("test".some)

    val p2 = pure(12)
    val out2 = ListInterpreter.execute(p2)
    out2 should be (List(12))

    val p3 = for {
      x <- pure(1)
      y <- pure(2)
      z <- pure("test")      
    } yield (x,y,z)
    ListInterpreter.execute(p3) should be (List((1,2,"test")))

    val program = for {
      x <- pure(12)
      c <- if (x < 11) pure(12.toString.some) else ask[Option[String]]("c")
      a <- interact[Int,String]("hiya", "in")
      b <- interact[Int,String]("hiya2", "in")
      _ <- tell[Option[String]]("_", c)
    } yield ((x, "test".some))

    ListInterpreter.execute(program) should be (List(12, "some"))
  }
}
