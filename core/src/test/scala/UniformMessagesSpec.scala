package ltbs.uniform

import org.scalatest._

class UniformMessagesSpec extends FunSpec with Matchers {

  // def argInsensitive[A](msg: UniformMessages[A]): Unit = {
  //   it("stuff") {
  //     1 shouldBe (1)      
  //   }
  // }

  describe("UniformMessages.fromMap") {
    val msg = UniformMessages.fromMap(Map("one.two.three" -> List(1), "four" -> List(Int.MinValue, Int.MaxValue)))

    it("provides a basic UniformMessages instance") {
      msg.get("one.two.three") shouldBe (Some(1))
      msg("one.two.three") shouldBe (1)
      msg.list("four") shouldBe (List(Int.MinValue, Int.MaxValue))
    }

    it("is insensitive to arguments") {
      msg("one.two.three") shouldBe (msg("one.two.three", 1, true, "test"))      
    }

//    argInsensitive(msg)
  }

  describe("UniformMessages.echo") {
    val msg = UniformMessages.echo

    it("returns what is sent, unless it is optional") {
      msg.get("one.two.three") shouldBe (None)
      msg("one.two.three") shouldBe ("one.two.three")
      msg.list("four") shouldBe (Nil)
    }

  }

  describe("UniformMessages.attentionSeeker") {
    val msg = UniformMessages.attentionSeeker

    it("returns what is sent, even if it is optional") {
      msg.get("one.two.three") shouldBe (Some("one.two.three"))
      msg("one.two.three") shouldBe ("one.two.three")
      msg.list("four") shouldBe (List("four"))
    }

  }


}
