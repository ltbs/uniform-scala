package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers

class BestGuessMessagesSpec extends AnyFlatSpec with Matchers {
  "BestGuessMessages" should "remove initial parts" in {
    BestGuessMessages.apply("one.two.three") shouldBe ("Three")
  }

  it should "process camel case" in {
    BestGuessMessages.apply("camelCaseSentence") shouldBe ("Camel Case Sentence")
  }

  it should "process title case" in {
    BestGuessMessages.apply("TitleCaseSentence") shouldBe ("Title Case Sentence")
  }

  it should "process hyphenated text" in {
    BestGuessMessages.apply("Hyphenated-Sentence") shouldBe ("Hyphenated Sentence")
  }

}
