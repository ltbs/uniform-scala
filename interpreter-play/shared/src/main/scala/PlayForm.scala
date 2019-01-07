package ltbs.uniform.interpreters.playframework

import ltbs.uniform.common.web._
import ltbs.uniform.datapipeline._

object PlayForm {
  def automatic[A](
    parser: DataParser[A],
    html: HtmlForm[A],
    messages: Messages
  ): PlayForm[A] = 
    sifProfunctor.lmap(
      UrlEncodedHtmlForm[A](parser, html, messages)
    )(_.body.asFormUrlEncoded.getOrElse(Map.empty))

}
