package ltbs.uniform

import validation.Rule
import scala.concurrent._
import cats.implicits._

case class FutureInterpreter()(implicit ec: ExecutionContext) extends MonadInterpreter[Future, Reader, Writer] {
  
  def askImpl[A](key: String, default: Option[A], validation: Rule[A], asker: Reader[A]): Future[A] = Future{

    @annotation.tailrec
    def inner(): A = {
      val input = scala.io.StdIn.readLine(s"$key [${asker.name}]: ")
      asker.read(input) match {
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

  def tellImpl[T](key: String,value: T,teller: Writer[T]): Future[Unit] = Future {
    println(teller.write(value))
  }

  override def endImpl(key: String): Future[Nothing] = Future {
    println(s"$key (ending)")
    System.exit(0)
    throw new IllegalStateException(key)
  }

}

object FutureInterpreterTestApp extends App {

  case class Coordinate(x: Int, y: Int)

  implicit val ec: ExecutionContext = ExecutionContext.global
  val i = FutureInterpreter()

  val journey2 = ask[Coordinate]("one").flatMap{x => tell("three", x)}

  val journey = for {
    r <- ask[Coordinate]("one")
    _ <- tell("three", r)
  } yield (r)

  implicit val r = new Reader[Int] {
    def name: String = "Number"
    def read(in: String): Either[String,Int] = Either.catchOnly[NumberFormatException] {
      in.toInt
    }.leftMap(_.getLocalizedMessage())
  }

  implicit val c = new Reader[Coordinate] {
    def name: String = "Comma separated coordinate"
    def read(in: String): Either[String,Coordinate] = {
      val (x::y::Nil) = in.split(",").toList
      Either.catchOnly[NumberFormatException] {
        Coordinate(x.trim.toInt, y.trim.toInt)
      }.leftMap(_.getLocalizedMessage())
    }
  }

  implicit val w = new Writer[Coordinate] {
    def write(v: Coordinate): String = s"x: ${v.x}, y: ${v.y}"
  }

  import scala.concurrent.duration._
  Await.result(
    i.execute(journey),
    100.minutes
  )
}
