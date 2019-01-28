package ltbs.uniform.prototype

import org.querki.jquery._
import cats.implicits._
import org.scalajs.dom.Element

trait MessagesProvider {

  var enabled = true
  var data: Map[String,String] = Map.empty
  val source = "file://./messages"

  def reload(): Unit = {
    def loadRemoteMessages(dataIn: Object, b: String, c: JQueryXHR): Any =
      parse(dataIn.toString)
    val remoteMessages: JQueryXHR = $.get(source, success=(loadRemoteMessages _), dataType="text")
  }

  implicit class RichMap[K,V](map: Map[K,V]) {
    def getFirst(keys: Iterable[K]): Option[V] = {
      @annotation.tailrec
      def inner(remaining: List[K]): Option[V] = {
        remaining match {
          case Nil => None
          case (k::ks) => map.get(k) match {
            case Some(v) => Some(v)
            case None    => inner(ks)
          }
        }
      }
      inner(keys.toList)
    }
  }

  def parse(in: String): Unit = {
    val newData = in.toString.lines.collect{
      _.split("=").toList match {
        case (key::value::_) => (key,value)
      }
    }.toMap

    if (data != newData) {
      println("applying changes")
      data = newData
      updateMessages()
    } else {
      println("no changes")
    }

  }

  def getMessage(keys: List[String], args: Any*): String = {
    data.getFirst(keys).getOrElse(keys.head)
  }

  def getMessage(key: String, args: Any*): String =
    getMessage(List(key), args:_*)

  def span(keySeq: Seq[String], args: Any*): String = {
    val allkeys = keySeq.mkString("|")
    val allargs = args.zipWithIndex.map{case (item, index) =>
      s"""i18nArg${index}="${item.toString}" """
    }.mkString
    s"""<span class="i18n" i18nKeys="$allkeys" $allargs>${keySeq.head}</span>"""
  }

  def span(key: String, args: Any*): String = span(List(key), args:_*)

  def updateMessages(): Unit = updateMessages($("body"))
  def updateMessages(elem: JQuery): Unit = {
    val elems = elem.find(".i18n")
    elems.each{elem: Element =>
      val keys = elem.getAttribute("i18nKeys").split("\\|")
      elem.innerHTML = if (enabled) getMessage(keys.toList) else keys.head
    }
    ()
  }
}
