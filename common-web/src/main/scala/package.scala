package ltbs.uniform

import cats.implicits._
import cats.arrow.Profunctor
import cats.Invariant
import play.twirl.api.Html

import java.net.URLEncoder.{encode => urlencode}
import java.net.URLDecoder.{decode => urldecode}

package object web {

  type FormUrlEncoded = Map[String, Seq[String]]
  type Input = Tree[String, List[String]]

  implicit def sifProfunctor[T] = new Profunctor[SimpleInteractionForm[?,T,?]] {
    def dimap[A, B, C, D](fab: SimpleInteractionForm[A,T,B])(f: C => A)(g: B => D) =
      new SimpleInteractionForm[C,T,D] {
        def decode(out: Encoded): Either[ErrorTree,T] = fab.decode(out)
        def receiveInput(data: C): Encoded = fab.receiveInput(f(data))
        def encode(in: T): Encoded = fab.encode(in)
        def render(key: String,existing: Option[Encoded], data: C, errors: ErrorTree): D =
          g(fab.render(key,existing,f(data), errors))
      }
  }

  implicit def sifInvariant[IN,OUT] = new Invariant[SimpleInteractionForm[IN,?,OUT]] {
    def imap[A, B](fa: SimpleInteractionForm[IN,A,OUT])(f: A => B)(g: B => A) =
      fa.transform(f map (_.asRight))(g)
  }

  def UrlEncodedHtmlForm[A](
    parser: DataParser[A],
    html: HtmlForm[A],
    messages: Messages
  ): SimpleInteractionForm[FormUrlEncoded,A,Html] = { 
    val underlying = new InputHtmlForm(parser, html, messages)
    sifProfunctor[A].lmap(underlying)(underlying.formToInput)
  }

  protected def decodeUrlString(input: String): FormUrlEncoded =
    input.split("&")
      .toList.map(_.trim).filter(_.nonEmpty)
      .groupBy(_.takeWhile(_ != '='))
      .map { case (k,v) =>
        val key = urldecode(k, "UTF-8").replace("[]", "")
        val value = v.map(x => urldecode(x.dropWhile(_ != '=').drop(1), "UTF-8"))
        (key, value)
      }

  protected def encodeUrlString(input: FormUrlEncoded): String =
    input.toList.flatMap{
      case (f,vs) => vs.map{v =>
        val encoded = urlencode(v, "UTF-8")
        s"$f=$encoded"
      }
    }.mkString("&")

  // TODO: No more bodged, non stack-safe tree implementations 
  protected[uniform] def encodeInput(key:String, in: Input): String = {
    def inner(subKey: String, subInput: Input): List[String] = {
      subInput.value.map{ v => s"$subKey=$v" } ++
      subInput.children.flatMap{
        case (k,f) => inner(s"${subKey}.$k", f)
      }
    }
    inner(key, in)
  }.mkString("&")

  protected[uniform] def decodeInput(in: String): Input = {
    val formEnc: FormUrlEncoded =
      in.split("&")
        .toList.map(_.trim).filter(_.nonEmpty)
        .groupBy(_.takeWhile(_ != '='))
        .mapValues(_.map(_.dropWhile(_ != '=').drop(1)))
    formToInput(formEnc)
  }

  protected def formToInput(in: FormUrlEncoded): Input = {

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

  protected[web] val required = "required"

}
