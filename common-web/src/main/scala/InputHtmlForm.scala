package ltbs.uniform.web

import cats.implicits._
import ltbs.uniform._
import play.twirl.api.Html
import java.net.URLDecoder.{decode => urldecode}

class InputHtmlForm[A](
  parser: DataParser[A],
  html: HtmlForm[A],
  messages: Messages
) extends SimpleInteractionForm[Input,A,Html] {

  def decode(out: Encoded): Either[ErrorTree,A] = {
    val urlEncoded = decodeUrlString(out)
    parser.bind(formToInput(urlEncoded).forestAtPath("root").getOrElse(Tree.empty))
  }

  def encode(in: A): Encoded = receiveInput(parser.unbind(in))
  def receiveInput(data: Input): Encoded = encodeInput("root", data)
  def render(key: String, existing: Option[Encoded], data: Input, errors: ErrorTree): Html =
    html.render(key, data, errors, messages)

  // TODO: No more bodged, non stack-safe tree implementations 
  private def encodeInput(key:String, in: Input): String = {
    def inner(subKey: String, subInput: Input): List[String] = {
      subInput.value.map{ v => s"$subKey=$v" } ++
      subInput.children.flatMap{
        case (k,f) => inner(s"${subKey}.$k", f)
      }
    }
    inner(key, in)
  }.mkString("&")

  private def decodeUrlString(input: String): FormUrlEncoded =
    input.split("&")
      .toList.map(_.trim).filter(_.nonEmpty)
      .groupBy(_.takeWhile(_ != '='))
      .map { case (k,v) =>
        val key = urldecode(k, "UTF-8").replace("[]", "")
        val value = v.map(x => urldecode(x.dropWhile(_ != '=').drop(1), "UTF-8"))
        (key, value)
      }

  protected[web] def formToInput(in: FormUrlEncoded): Input = {

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
}
