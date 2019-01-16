package playexample

import org.scalatest._
import ltbs.uniform.web._
import ltbs.uniform.web.parser._, InferParser._
import ltbs.uniform.widgets.govuk._
import ltbs.uniform.interpreters.playframework._
import ltbs.uniform.sampleprograms.BeardTax._

class FormConstructionSpec extends FlatSpec with Matchers {

  implicit val messages: Messages = NoopMessages

  val form = PlayForm.automatic[Option[MemberOfPublic]]
  val baseForm = UrlEncodedHtmlForm[Option[MemberOfPublic]](implicitly, implicitly, implicitly)
  val luke = MemberOfPublic("Luke","Tebbs",java.time.LocalDate.of(1978,12,1))
  val playHtml = form.render("is-public", Some(form.encode(Some(luke))), dummyGet("/is-public"))
  val baseHtml = baseForm.render("is-public", Some(baseForm.encode(Some(luke))), Map.empty[String, List[String]])

  "A PlayForm for Option[MemberOfPublic]" should "render the same as for UrlEncodedHtmlForm" in {
    playHtml should be (baseHtml)
  }

  it should "contain supplied values" in {
    playHtml.toString should include ("Luke")
  }
}
