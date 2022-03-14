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

case class ConsoleInteract[T,A](teller: ConsoleTell[T], asker: ConsoleAsk[A])

object ConsoleInteract {
  implicit def autoInteract[T,A](implicit teller: ConsoleTell[T], asker: ConsoleAsk[A]): ConsoleInteract[T,A] =
    ConsoleInteract(teller, asker)
}

case class ConsoleInterpreter() extends MonadInterpreter[IO, ConsoleInteract, ConsoleAsk] {

  def monadInstance = implicitly[cats.Monad[IO]]

  override def interactImpl[T, A](
    key: String,
    tellValue: T,
    default: Option[A],
    validation: ltbs.uniform.validation.Rule[A],
    customContent: Map[String,(String, List[Any])],
    interaction: ltbs.uniform.ConsoleInteract[T,A]
  ): cats.effect.IO[A] =
    interaction.teller(key, tellValue) >>
      interaction.asker(key, default, validation)

  override def askListImpl[A](
    key: String,
    askJourney: (Option[Int], List[A]) => IO[A],
    deleteJourney: (Int, List[A]) => IO[Boolean],
    default: Option[List[A]], 
    validation: Rule[List[A]],
    customContent: Map[String,(String,List[Any])],    
    asker: ConsoleAsk[A]
  ): IO[List[A]] = ???
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
    three <- convert(Future.successful(3000))
    _ <- convert(12.asRight[String])
    _ <- convert(Future.successful("five"))
    _ <- tell("stringout", "stringTest")
    one <- ask[Int]("one")
    _ <- interact[Boolean]("one", 12)    
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

  implicit val tellString: ConsoleTell[String] = ???
  implicit val askUnit: ConsoleAsk[Unit] = ???  

  def run(args: List[String]): IO[ExitCode] = {
    val r: IO[Int] = ConsoleInterpreter().interpret(program)
    r.map{println}.as(ExitCode(0))
  }
}
