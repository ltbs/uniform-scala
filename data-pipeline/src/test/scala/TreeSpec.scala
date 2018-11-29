package ltbs.uniform.datapipeline

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {
  val in: FormUrlEncoded = Map("one.two.three" -> Seq("findme"))
  formToInput(in).atPath("one.two.three".split("[.]"):_*) should be (Some(Seq("findme")))
}
