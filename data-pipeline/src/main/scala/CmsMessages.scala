package ltbs.uniform.datapipeline

import play.twirl.api.Html
import Messages.htmlRead

case class CmsMessages(underlying: Messages[String]) extends Messages[Html] {

  private def wrap(
    key: String,
    args: Seq[Any],
    clazz: String = ""
  )(in: => String): Html =
    Html(s"""<span class="uniform-cms $clazz" key="$key">$in</span>""")

  def get(key: String, args: Any*): Option[Html] =
    underlying.get(key, args:_*).map(wrap(key, args)(_)) orElse
  Some(wrap(key, args, "uniform-cms-none")("(empty)"))

  def get(key: List[String], args: Any*): Option[Html] =
    underlying.get(key, args:_*).map(wrap(key.mkString(","), args)(_)) orElse
  Some(wrap(key.mkString(","), args, "uniform-cms-none")("(empty)"))

  def list(key: String, args: Any*): List[Html] =
    (underlying.list(key, args:_*) :+ "add").map(wrap(key, args)(_)) :+
  wrap(key.mkString(","), args, "uniform-cms-add")("(add)")

  def keyValuePair(key: String, args: Any*): List[(Html,Html)] =
    underlying.keyValuePair(key, args:_*).map{
      case (k,v) => (Html(k),Html(v))
    } :+
  ((
    wrap(key.mkString(","), args, "uniform-cms-add")("(add)"),
    wrap(key.mkString(","), args, "uniform-cms-add")("(add)")
  ))


}
