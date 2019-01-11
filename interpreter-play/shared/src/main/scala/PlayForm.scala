package ltbs.uniform.interpreters.playframework

import ltbs.uniform.web._

object PlayForm {
  def automatic[A]( implicit
    parser: DataParser[A],
    html: HtmlForm[A],
    messages: Messages
  ): PlayForm[A] = 
    sifProfunctor.lmap(
      UrlEncodedHtmlForm[A](parser, html, messages)
    ){ request =>

      val urlEncodedData =
        request.body.asFormUrlEncoded.getOrElse(Map.empty)
      val (first: String,_) =
        urlEncodedData.find(_._1 != "csrfToken").getOrElse(("",""))
      val key = first.takeWhile(_ != '.')
      urlEncodedData.forestAtPath(key)
    }

}
