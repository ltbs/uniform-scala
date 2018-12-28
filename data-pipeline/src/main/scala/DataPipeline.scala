package ltbs.uniform

import cats.implicits._
import cats.{Invariant,Monoid}
import java.net.URLEncoder.{encode => urlencode}
import java.net.URLDecoder.{decode => urldecode}
import java.time.LocalDate
import enumeratum._
import play.twirl.api.Html

package object datapipeline {

  type FormUrlEncoded = Map[String, Seq[String]]
  type ErrorTree = Tree[String,String]
  type Input = Tree[String, List[String]]
  type Error = Tree[String, String]

  implicit val htmlMonoidInstance = new Monoid[Html] {
    def empty: Html = Html("")
    def combine(a: Html, b: Html):Html = Html(a.toString ++ b.toString)
  }

  implicit def treeMonoid[K, V: Monoid] = new Monoid[Tree[K,V]] {
    def empty: Tree[K,V] = Tree(Monoid[V].empty)

    def combine(x: Tree[K,V], y: Tree[K,V]): Tree[K,V] = {
      val value = x.value |+| y.value

      // crude 'unionwith'
      val xkeys = x.children.keys.toList
      val ykeys = y.children.keys.toList
      val shared = xkeys.intersect(ykeys)
      val xonly = xkeys.map{v => v -> x.children(v)}
      val yonly = ykeys.map{v => v -> y.children(v)}
      val merged = shared.map{v => v -> combine(x.children(v), y.children(v))}

      Tree(value, Map({xonly ++ yonly ++ merged}:_*))
    }
  }

  def decodeUrlString(input: String): FormUrlEncoded =
    input.split("&")
      .toList.map(_.trim).filter(_.nonEmpty)
      .groupBy(_.takeWhile(_ != '='))
      .map { case (k,v) =>
        val key = urldecode(k, "UTF-8").replace("[]", "")
        val value = v.map(x => urldecode(x.dropWhile(_ != '=').drop(1), "UTF-8"))
        (key, value)
      }

  def encodeUrlString(input: FormUrlEncoded): String =
    input.toList.flatMap{
      case (f,vs) => vs.map{v =>
        val encoded = urlencode(v, "UTF-8")
        s"$f=$encoded"
      }
    }.mkString("&")

  // TODO: No more bodged, non stack-safe tree implementations 
  def encodeInput(key:String, in: Input): String = {
    def inner(subKey: String, subInput: Input): List[String] = {
      subInput.value.map{ v => s"$subKey=$v" } ++
      subInput.children.flatMap{
        case (k,f) => inner(s"${subKey}.$k", f)
      }
    }
    inner(key, in)
  }.mkString("&")

  def decodeInput(in: String): Input = {
    val formEnc: FormUrlEncoded =
      in.split("&")
        .toList.map(_.trim).filter(_.nonEmpty)
        .groupBy(_.takeWhile(_ != '='))
        .mapValues(_.map(_.dropWhile(_ != '=').drop(1)))
    formToInput(formEnc)
  }

  def formToInput(in: FormUrlEncoded): Input = {

    val depth: List[(List[String], Seq[String])] =
      in.toList.map{
        case (k,v) => (k.split("[.]").toList.filter(_.nonEmpty),v)
      }

    def peel(
      input: List[(List[String], Seq[String])]
    ): List[(List[String], Seq[String])] = input collect {
      case (_::xs, v) => (xs,v)
    }

    def grouped(
      input: List[(List[String], Seq[String])]
    ): Map[Option[String],List[(List[String], Seq[String])]] = 
      input.groupBy(_._1.headOption)

    def makeTree(input: Map[Option[String],List[(List[String], Seq[String])]]): Input = {
      val root = input.get(None).toList.flatMap(_.flatMap(_._2))
      val children = input collect {
        case (Some(k), other) =>
          val otherGrouped: Map[Option[String],List[(List[String], Seq[String])]] = grouped(peel(other))
          (k,makeTree(otherGrouped))
      }
      Tree(root, children.toMap)
    }
    makeTree(grouped(depth))
  }

  val required = "required"

  implicit def stringParser: DataParser[String] = new DataParser[String] {
    def bind(in: Input): Either[Error,String] = in.value match {
      case Nil => Tree("required").asLeft
      case empty::Nil if empty.trim == "" => Tree("required").asLeft
      case s::Nil => s.asRight
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: String): Input = Tree(List(a))
  }

  implicit def intParser: DataParser[Int] = new DataParser[Int] {
    def bind(in: Input): Either[Error,Int] = in.value match {
      case ""::Nil | Nil => Tree("required").asLeft
      case s::Nil => Either.catchOnly[NumberFormatException](s.toInt)
          .leftMap(_ => Tree("nonnumericformat"))
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Int): Input = Tree(List(a.toString))
  }

  implicit def longParser: DataParser[Long] = new DataParser[Long] {
    def bind(in: Input): Either[Error,Long] = in.value match {
      case ""::Nil | Nil => Left(Tree("required"))
      case s::Nil => Either.catchOnly[NumberFormatException](s.toLong)
          .leftMap(_ => Tree("nonnumericformat"))
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Long): Input = Tree(List(a.toString))
  }

  implicit def booleanParser: DataParser[Boolean] = new DataParser[Boolean] {
    def bind(in: Input): Either[Error,Boolean] = in.value match {
      case t::Nil if t.toUpperCase == "TRUE" => true.asRight
      case f::Nil if f.toUpperCase == "FALSE" => false.asRight
      case Nil => Tree(required).asLeft
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Boolean): Input = Tree(List(a.toString.toUpperCase))
  }

  implicit def localDateParser: DataParser[LocalDate] = new DataParser[LocalDate] {

    def bind(in: Input): Either[Error,LocalDate] = {
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
      def bind(in: Input): Either[Error,A] = stringParser.bind(in) >>= { x =>
        Either.catchOnly[NoSuchElementException](enum.withName(x)).leftMap{_ => Tree("badValue")}
      }
      def unbind(a: A): Input = Tree(List(a.toString))
    }

  implicit def enumeratumSetParser[A <: EnumEntry](implicit enum: Enum[A]): DataParser[Set[A]] =
    new DataParser[Set[A]] {
      def bind(in: Input): Either[Error,Set[A]] = {
        in.value.map{ x =>
          Either.catchOnly[NoSuchElementException](enum.withName(x)).leftMap{_ => Tree("badValue"): Error}
        }.sequence.map{_.toSet}
      }
      def unbind(a: Set[A]): Input = Tree(a.map(_.toString).toList)
    }


  implicit def optionParser[A](implicit subpipe: DataParser[A]): DataParser[Option[A]] =
    new DataParser[Option[A]] {
      def bind(in: Input): Either[Error,Option[A]] = {
        val outer: Either[Error,Boolean] = in.get("outer").flatMap(booleanParser.bind)
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
      def bind(in: Input): Either[Error,B] = fa.bind(in).map(f)
      def unbind(a:B): Input = fa.unbind(g(a))
    }
  }

}
