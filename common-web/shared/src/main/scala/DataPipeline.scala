package ltbs.uniform.web

import cats.implicits._
import cats.Invariant
import java.time.LocalDate
import enumeratum._
import ltbs.uniform.Tree
import ltbs.uniform._

package object parser {

  implicit def stringParser: DataParser[String] = new DataParser[String] {
    def bind(in: Input): Either[ErrorTree,String] = in.value match {
      case Nil => Tree("required").asLeft
      case empty::Nil if empty.trim == "" => Tree("required").asLeft
      case s::Nil => s.asRight
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: String): Input = Tree(List(a))
  }

  implicit def intParser: DataParser[Int] = new DataParser[Int] {
    def bind(in: Input): Either[ErrorTree,Int] = in.value match {
      case ""::Nil | Nil => Tree("required").asLeft
      case s::Nil => Either.catchOnly[NumberFormatException](s.toInt)
          .leftMap(_ => Tree("nonnumericformat"))
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Int): Input = Tree(List(a.toString))
  }

  implicit def longParser: DataParser[Long] = new DataParser[Long] {
    def bind(in: Input): Either[ErrorTree,Long] = in.value match {
      case ""::Nil | Nil => Left(Tree("required"))
      case s::Nil => Either.catchOnly[NumberFormatException](s.toLong)
          .leftMap(_ => Tree("nonnumericformat"))
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Long): Input = Tree(List(a.toString))
  }

  implicit def booleanParser: DataParser[Boolean] = new DataParser[Boolean] {
    def bind(in: Input): Either[ErrorTree,Boolean] = in.value match {
      case t::Nil if t.toUpperCase == "TRUE" => true.asRight
      case f::Nil if f.toUpperCase == "FALSE" => false.asRight
      case Nil => Tree(required).asLeft
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Boolean): Input = Tree(List(a.toString.toUpperCase))
  }

  implicit def localDateParser: DataParser[LocalDate] = new DataParser[LocalDate] {

    def bind(in: Input): Either[ErrorTree,LocalDate] = {
      def numField(key: String) =
        (in.get(key) >>= intParser.bind).leftMap{ x => 
          Tree("", Map(key -> x))
        }.toValidated

      (
        numField("year"),
        numField("month"),
        numField("day")
      ).tupled.toEither.flatMap{ case (y,m,d) =>
        Either.catchOnly[java.time.DateTimeException]{
          LocalDate.of(y,m,d)
        }.leftMap(_ => Tree("badDate"))
      }
    }

    def unbind(a: LocalDate): Input = Tree(
      Nil,
      Map(
        "year" -> Tree(List(a.getYear.toString)),
        "month" -> Tree(List(a.getMonthValue.toString)),
        "day" -> Tree(List(a.getDayOfMonth.toString)))
    )
  }

  implicit def enumeratumParser[A <: EnumEntry](implicit enum: Enum[A]): DataParser[A] =
    new DataParser[A] {
      def bind(in: Input): Either[ErrorTree,A] = stringParser.bind(in) >>= { x =>
        Either.catchOnly[NoSuchElementException](enum.withName(x)).leftMap{_ => Tree("badValue")}
      }
      def unbind(a: A): Input = Tree(List(a.toString))
    }

  implicit def enumeratumSetParser[A <: EnumEntry](implicit enum: Enum[A]): DataParser[Set[A]] =
    new DataParser[Set[A]] {
      def bind(in: Input): Either[ErrorTree,Set[A]] = {
        in.value.map{ x =>
          Either.catchOnly[NoSuchElementException](enum.withName(x)).leftMap{_ => Tree("badValue"): ErrorTree}
        }.sequence.map{_.toSet}
      }
      def unbind(a: Set[A]): Input = Tree(a.map(_.toString).toList)
    }


  implicit def optionParser[A](implicit subpipe: DataParser[A]): DataParser[Option[A]] =
    new DataParser[Option[A]] {
      def bind(in: Input): Either[ErrorTree,Option[A]] = {
        val outer: Either[ErrorTree,Boolean] = in.get("outer").flatMap(booleanParser.bind)
        outer >>= {x =>
          if (x) {
            in.get("inner") >>= {
              inner => subpipe.bind(inner).bimap(x => Tree("", Map("inner" -> x)), _.some)
            }
          }
          else none[A].asRight
        }
      }

      def unbind(a: Option[A]): Input = a match {
        case None => Tree(List(""), Map("outer" -> booleanParser.unbind(false)))
        case Some(x) => Tree(List(""), Map("outer" -> booleanParser.unbind(true),
                                           "inner" -> subpipe.unbind(x)))
      }
    }

  implicit val parserInvariant = new Invariant[DataParser] {
    def imap[A, B](fa: DataParser[A])(f: A => B)(g: B => A): DataParser[B] = new DataParser[B] {
      def bind(in: Input): Either[ErrorTree,B] = fa.bind(in).map(f)
      def unbind(a:B): Input = fa.unbind(g(a))
    }
  }

}
