package ltbs.uniform

import validation.Rule
import cats.implicits._
import cats.effect._
import izumi.reflect.Tag

trait ConsoleAsk[A] {

  def name: String
  def read(in: String): Either[String, A]  
  def apply(
    key: String,
    default: Option[A],
    validation: Rule[A]
  ): IO[A] = IO {

    @annotation.tailrec
    def inner(): A = {
      val input = scala.io.StdIn.readLine(s"$key [$name]: ")
      read(input) match {
        case Left(error) =>
            println(error)
            inner()
        case Right(v) =>
          validation(v) match {
            case cats.data.Validated.Valid(o) => o
            case cats.data.Validated.Invalid(e) =>
              e.map{println}
              inner()
          }
      }
    }
    
    inner()
  }
}

object ConsoleAsk {
  def apply[A: Tag](f: String => Either[String,A]) = new ConsoleAsk[A] {
    def read(in: String) = f(in)
    def name: String = implicitly[Tag[A]].tag.shortName
  }
}

trait ConsoleTell[A] {

  def format(in: A): String

  def apply(
    key: String,
    value: A
  ): IO[Unit] = IO { println(s"$key : ${format(value)}") }
}

object ConsoleTell {
  implicit def anyInstance[A] = new ConsoleTell[A] {
    def format(in: A): String = in.toString
  }
}

case class ConsoleInterpreter() extends MonadInterpreter[IO, ConsoleAsk, ConsoleTell] {

  def monadInstance = implicitly[cats.Monad[IO]]

  def askImpl[A](
    key: String,
    default: Option[A],
    validation: Rule[A],
    customContent: Map[String,(String,List[Any])],        
    asker: ConsoleAsk[A]
  ): IO[A] =
    asker(key, default, validation)

  def tellImpl[T](
    key: String,
    value: T,
    customContent: Map[String,(String,List[Any])],    
    teller: ConsoleTell[T]
  ): IO[Unit] =
    teller(key, value)

  override def endImpl(
    key: String,
    customContent: Map[String,(String,List[Any])]    
  ): IO[Nothing] = {
    IO { throw new IllegalStateException("end of journey") }
  }

}

object ConsoleApp extends IOApp {

  import scala.concurrent._
  import cats.~>
  val three = {
    Future.successful(3)
  }

  implicit val futureToIO = new (Future ~> IO){
    def apply[A](in: Future[A]): IO[A] = IO.fromFuture(IO(in))
  }

  implicit val eitherStringToIO = new (Either[String, ?] ~> IO){
    def apply[A](in: Either[String, A]): IO[A] = in match {
      case Left(err) => IO.raiseError(new IllegalStateException(err))
      case Right(v) => IO.pure(v)
    }
  }

  val program = for {
    three <- convert(Future.successful(3))
    stringTest2 <- convert("four".asRight[String])
    stringTest <- convert(Future.successful("three"))
    _ <- tell("stringout", "stringTest")
    one <- ask[Int]("one")
    blah <- interact[Boolean]("one", 12)    
    two <- ask[Int]("two")    
  } yield (one + two + three)

  implicit val askInt = ConsoleAsk{in =>
    Either.catchOnly[NumberFormatException](in.toInt).leftMap(
      _.getLocalizedMessage()
    )
  }

  implicit val askBool = ConsoleAsk{in =>
    in match {
      case "true" => Right(true)
      case _ => Right(false)
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    val r: IO[Int] = ConsoleInterpreter().interpret(program)
    r.map{println}.as(ExitCode(0))
  }
}
