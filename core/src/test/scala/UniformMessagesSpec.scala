package ltbs.uniform

class UniformMessagesSpec extends munit.FunSuite {

  // def argInsensitive[A](msg: UniformMessages[A]): Unit = {
  //   it("stuff") {
  //     1 shouldBe (1)      
  //   }
  // }

  test("UniformMessages.fromMap") {
    val msg = UniformMessages.fromMap(Map("one.two.three" -> List(1), "four" -> List(Int.MinValue, Int.MaxValue)))

    test("provides a basic UniformMessages instance") {
      assertEquals(msg.get("one.two.three"), Some(1))
      assertEquals(msg("one.two.three"), 1)
      assertEquals(msg.list("four"), List(Int.MinValue, Int.MaxValue))
    }

    test("is insensitive to arguments") {
      assertEquals(msg("one.two.three"), msg("one.two.three", 1, true, "test"))
    }

//    argInsensitive(msg)
  }

  test("UniformMessages.echo") {
    val msg = UniformMessages.echo

    test("returns what is sent, unless it is optional") {
      assertEquals(msg.get("one.two.three"), None)
      assertEquals(msg("one.two.three"), "one.two.three")
      assertEquals(msg.list("four"), Nil)
    }

  }

  test("UniformMessages.attentionSeeker") {
    val msg = UniformMessages.attentionSeeker

    test("returns what is sent, even if it is optional") {
      assertEquals(msg.get("one.two.three"), Some("one.two.three"))
      assertEquals(msg("one.two.three"), "one.two.three")
      assertEquals(msg.list("four"), List("four"))
    }

  }


}
