package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import cats.implicits._
import cats.{Id, Monad}
import shapeless.{Id => _, _}
import scala.language.higherKinds
import validation.Rule
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag

trait Example[+A] { val value: A }

trait Reader[+A] {
  def name: String
  def read(in: String): Either[String, A]
}

trait Writer[T] {
  def write(v: T): String
}

object ListInterpreter extends MonadInterpreter[List, Example, Noop] {

  def askImpl[A](key: String, default: Option[A], validation: Rule[A], asker: Example[A]): List[A] = List(asker.value)
  def tellImpl[T](key: String, value: T, teller: Noop[T]): List[Unit] = {
    println(value.toString)
    List(())
  }

  override def endImpl(key: String): List[Nothing] = Nil
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
    implicit val os: Option[Int] = Option(12)
    ListInterpreter.execute(program) should be (List((12, Some("test"))))
  }
}
