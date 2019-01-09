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

      val urlEncodedData = request.body.asFormUrlEncoded.getOrElse(Map.empty)
      println("urlEncodedData: " + urlEncodedData)
      val (first: String,_) = urlEncodedData.find(_._1 != "csrfToken").getOrElse(("",""))
      val key = first.takeWhile(_ != '.')
      println("key: " + key)
      request.body.asFormUrlEncoded.getOrElse(Map.empty).map { case (k,v) => 
        (k.replaceFirst(key,""),v)
      }
    }

}
