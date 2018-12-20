package ltbs.uniform.datapipeline

import play.twirl.api.Html
import Messages.htmlRead

case class CmsMessages(underlying: Messages[String]) extends Messages[Html] {

  private def wrap(key: String, args: Seq[Any])(in: => String): Html =
    Html(s"""<span class="uniform-cms" key="$key">$in</span>""")

  def get(key: String, args: Any*): Option[Html] =
    underlying.get(key, args:_*).map(wrap(key, args)(_))

  def get(key: List[String], args: Any*): Option[Html] =
    underlying.get(key, args:_*).map(wrap(key.mkString(","), args)(_))

  def list(key: String, args: Any*): List[Html] =
    underlying.list(key, args:_*).map(wrap(key, args)(_))

}
