package ltbs.uniform

import cats.implicits._
import cats.data.Validated
import cats.Invariant
import java.net.URLEncoder.{encode => urlencode}
import java.net.URLDecoder.{decode => urldecode}

package object datapipeline {

  type FormUrlEncoded = Map[String, Seq[String]]

  def decodeUrlString(input: String): FormUrlEncoded =
    input.split("&")
      .toList.map(_.trim).filter(_.nonEmpty)
      .groupBy(_.takeWhile(_ != '='))
      .mapValues(_.map(x => urldecode(x.dropWhile(_ != '=').tail, "UTF-8")))

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
        .mapValues(_.map(_.dropWhile(_ != '=').tail))
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

  type Input = Tree[String, List[String]]
  type Error = Tree[String, String]
  type Pipeline[A] = Input => Either[Error,A]

  val required = "required"

  implicit def stringPipeline: Pipeline[String] = _.value match {
    case s::Nil => s.asRight
    case _ => Tree("badValue").asLeft
  }

  implicit def stringParser: DataParser[String] = new DataParser[String] {
    def bind(in: Input): Either[Error,String] = in.value match {
      case s::Nil => s.asRight
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: String): Input = Tree(List(a))
  }

  implicit def intParser: DataParser[Int] = new DataParser[Int] {
    def bind(in: Input): Either[Error,Int] = in.value match {
      case s::Nil => Either.catchOnly[NumberFormatException](s.toInt)
          .leftMap(_ => Tree("nonnumericformat"))
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Int): Input = Tree(List(a.toString))
  }

  implicit def longParser: DataParser[Long] = new DataParser[Long] {
    def bind(in: Input): Either[Error,Long] = in.value match {
      case s::Nil => Either.catchOnly[NumberFormatException](s.toLong)
          .leftMap(_ => Tree("nonnumericformat"))
      case _ => Tree("badValue").asLeft
    }

    def unbind(a: Long): Input = Tree(List(a.toString))
  }

  implicit def booleanPipeline: Pipeline[Boolean] = {
    _.value match {
      case t::Nil if t.toUpperCase == "TRUE" => true.asRight
      case f::Nil if f.toUpperCase == "FALSE" => false.asRight
      case Nil => Tree(required).asLeft
      case _ => Tree("badValue").asLeft
    }
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

  implicit def optionPipeline[A](implicit subpipe: Pipeline[A]): Pipeline[Option[A]] = { m =>
    val outer: Either[Error,Boolean] = m.get("outer").flatMap(booleanPipeline)
    outer >>= {x =>
      if (x) {
        m.get("inner") >>= {
          inner => subpipe(inner).bimap(x => Tree("", Map("inner" -> x)), _.some)
        }
      }
      else none[A].asRight
    }
  }

  implicit def optionParser[A](implicit subpipe: DataParser[A]): DataParser[Option[A]] = new DataParser[Option[A]] {
    def bind(in: Input): Either[Error,Option[A]] = {
      val outer: Either[Error,Boolean] = in.get("outer").flatMap(booleanPipeline)
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
      case Some(x) => Tree(List(""), Map("outer" -> booleanParser.unbind(false),
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
