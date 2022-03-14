package ltbs.uniform

import cats.implicits._
import validation.Rule

trait Example[T, +A] { val value: A }

trait Reader[+A] {
  def name: String
  def read(in: String): Either[String, A]
}

trait Writer[T] {
  def write(v: T): String
}

object ListInterpreter extends MonadInterpreter[List, Example, Noop] {

  def monadInstance = implicitly[cats.Monad[List]]

  override def interactImpl[T, A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: ltbs.uniform.validation.Rule[A],
    customContent: Map[String,(String, List[Any])],
    interaction: ltbs.uniform.Example[T,A]
  ): List[A] = {
    println(tellValue)
    List(interaction.value)
  }

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => List[A],
    deleteJourney: (Int, List[A]) => List[Boolean],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String,List[Any])],    
    asker: Noop[A]
  ): List[List[A]] = askJourney(None, Nil).replicateA(3)
}

class TestInterpreter3 extends munit.FunSuite {

  def example[A](x: A): Example[Unit, A] = new Example[Unit, A] { val value: A = x }  
  def exampleInteract[T, A](x: A): Example[T, A] = new Example[T, A] { val value: A = x }
  implicit def noop[A]: Noop[A] = new Noop[A] {}

  implicit val exInt = example[Int](1)
  implicit val exIntTuple = example((1,1))
  implicit val exIntList = example(List(1))
  implicit val exOptString = example("test".some)
  implicit val exTellOptString = exampleInteract[Option[String], Unit](())
  implicit val exIntStringInt = exampleInteract[String, Int](1)

  test ("step1") {


    val p2 = pure(12)
    val out2 = ListInterpreter.interpret(p2)    
    assertEquals(out2, List(12))
  }
  test ("step2") {
    val p3: Uniform[Needs[_,_],Any,(Int, Int, String)] = for {
      x <- pure(1)
      y <- pure(2)
      z <- pure("test")      
    } yield (x,y,z)
    assertEquals(ListInterpreter.interpret(p3), List((1,2,"test")))
  }

  test ("step3") {
    val program = for {
      x <- interact[Int]("x",())
    } yield ((12 * x, "test".some))

    assertEquals(ListInterpreter.interpret(program), List((12, Some("test"))))
  }
}

