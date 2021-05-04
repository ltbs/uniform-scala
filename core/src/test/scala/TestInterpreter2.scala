package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import cats.implicits._
import validation.Rule

trait Example[+A] { val value: A }

trait Reader[+A] {
  def name: String
  def read(in: String): Either[String, A]
}

trait Writer[T] {
  def write(v: T): String
}

object ListInterpreter extends MonadInterpreter[List, Example, Noop, Noop] {

  def monadInstance = implicitly[cats.Monad[List]]

  override def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],
    asker: Example[A]
  ): List[A] = List(asker.value)

  override def tellImpl[T](
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])],
    teller: Noop[T]
  ): List[Unit] = {
    println(value.toString)
    List(())
  }

  override def endImpl(
    key: String,
    customContent: Map[String,(String,List[Any])]
  ): List[Nothing] = Nil


  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => List[A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String,List[Any])],    
    asker: Noop[A]
  ): List[List[A]] = askJourney(None, Nil).replicateA(3)
}

class TestInterpreter3 extends AnyFlatSpec with Matchers {

  def example[A](x: A): Example[A] = new Example[A] { val value: A = x }
  implicit def noop[A]: Noop[A] = new Noop[A] {}

  "A simple interpreter" should "be able to compose Id monad instances" in {

    implicit val exInt = example[Int](1)
    implicit val exIntTuple = example((1,1))    
    implicit val exIntList = example(List(1))    
    implicit val exOptString = example("test".some)

    val p2 = pure(12)
    val out2 = ListInterpreter.interpret(p2)    
    out2 should be (List(12))

    val p3 = for {
      x <- pure(1)
      y <- pure(2)
      z <- pure("test")      
    } yield (x,y,z)
    ListInterpreter.interpret(p3) should be (List((1,2,"test")))

    val program = for {
      x <- pure(12)
      c <- if (x < 11) pure(12.toString.some) else ask[Option[String]]("c")
      a <- interact[Int]("hiya", "in")
      b <- interact[Int]("hiya2", "in")
      // l <- ask[List[(Int, Int)]]("coordinates")
      // l2 <- askList("coordinates2")( (editIndex, existing: List[(Int, Int)]) => {
      //   val editRow: Option[(Int, Int)] = editIndex.map(existing.apply)
      //   for {
      //     x <- ask[Int]("x", default = editRow.map(_._1))
      //     y <- ask[Int]("y", default = editRow.map(_._2))
      //   } yield (x,y)
      // })
      _ <- tell[Option[String]]("_", c)
    } yield ((x, "test".some))

    ListInterpreter.interpret(program) should be (List((12, Some("test"))))
  }
}

