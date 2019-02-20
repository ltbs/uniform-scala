package ltbs.uniform.interpreters.playframework

import ltbs.uniform.web._
import play.twirl.api.Html

object PlayForm {
  def automatic[TELL,ASK]( implicit
    parser: DataParser[ASK],
    html: HtmlForm[ASK],
    messages: Messages,
    renderTell: (TELL, String) => Html
  ): PlayForm[TELL,ASK] = 
    sifProfunctor.lmap(
      UrlEncodedHtmlForm[TELL,ASK](parser, html, renderTell, messages)
    ){ request =>

      val urlEncodedData =
        request.body.asFormUrlEncoded.getOrElse(Map.empty)
      val (first: String,_) =
        urlEncodedData.find(_._1 != "csrfToken").getOrElse(("",""))
      val key = first.takeWhile(_ != '.')
      urlEncodedData.forestAtPath(key)
    }

}
