package ltbs.uniform

class BestGuessMessagesSpec extends munit.FunSuite {
  test("BestGuessMessages.apply") {
    assertEquals(BestGuessMessages.apply("remove.initial.parts"), "Parts")
    assertEquals(BestGuessMessages.apply("camelCaseSentence"), "Camel Case Sentence")
    assertEquals(BestGuessMessages.apply("TitleCaseSentence"), "Title Case Sentence")
    assertEquals(BestGuessMessages.apply("Hyphenated-Sentence"), "Hyphenated Sentence")
  }
}
