package ltbs.uniform
package interpreters.playframework

import common.web._
import play.twirl.api.Html
import collection.immutable.ListMap

trait InferTellTwirlDL extends InferTell[Html] {

  def mapToHtml(m: ListMap[List[String],Html], messages: UniformMessages[Html]): Html = {
    val listedItems = m.map {
      case (k, v) => s"""<dt>${k.mkString(".")}</dt><dd>${v.toString}</dd>"""
    }.mkString

    Html("<dl>" ++ listedItems ++ "</dl>")
  }
    
  implicit val stringFieldFunction: FieldFunction[String] = {case (x, _) => ListMap(List.empty[String] -> Html(x))}
}
