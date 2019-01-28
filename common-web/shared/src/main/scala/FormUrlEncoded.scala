package ltbs.uniform.web

import ltbs.uniform.Tree
import java.net.URLEncoder.{encode => urlencode}
import java.net.URLDecoder.{decode => urldecode}

class RichFormUrlEncoded(input: FormUrlEncoded) {
  def writeString: String = input.toList.flatMap{
    case (f,v::Nil) => List(
      s"$f=${urlencode(v, "UTF-8")}"
    )
    case (f,vs) => vs.map{v =>
      val encoded = urlencode(v, "UTF-8")
      s"$f[]=$encoded"
    }
  }.mkString("&")

  def toInputTree: Input = {

    val depth: List[(List[String], Seq[String])] =
      input.toList.map{
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

  def forestAtPath(path: String*): FormUrlEncoded =
    input.toInputTree.forestAtPath(path:_*)
      .fold(Map.empty[String,Seq[String]])(FormUrlEncoded.fromInputTree)

  def prefix(prefix: String): FormUrlEncoded =
    input.toList.map{case (k,v) =>
      s"$prefix.$k" -> v
    }.toMap

}

object FormUrlEncoded {

  def readString(input: String): FormUrlEncoded =
    input.split("&")
      .toList.map(_.trim).filter(_.nonEmpty)
      .groupBy(_.takeWhile(_ != '='))
      .map { case (k,v) =>
        val key = urldecode(k, "UTF-8").replace("[]", "")
        val value = v.map(x => urldecode(x.dropWhile(_ != '=').drop(1), "UTF-8"))
        (key, value)
      }

  def fromInputTree(input: Input): FormUrlEncoded = {

    // TODO: Remove this shameful disgusting code and replace it with
    // something that is stack-safe and doesn't convert to a string
    // representation and then back again
    def encodeInput(key:List[String], in: Input): String = {
      def inner(subKey: List[String], subInput: Input): List[String] = {

        {
          if (subInput.value.size > 1)
            subInput.value.map{ v => s"${subKey.mkString(".")}[]=$v" }
          else
            subInput.value.map{ v => s"${subKey.mkString(".")}=$v" }
        } ++ subInput.children.flatMap{
          case (k,f) => inner(subKey :+ k, f)
        }
      }
      inner(key, in)
    }.mkString("&")

    readString(encodeInput(Nil,input))
  }
}
