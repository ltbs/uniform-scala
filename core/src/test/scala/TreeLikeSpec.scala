package ltbs.uniform

import cats.data.NonEmptyList
import cats.implicits._
import collection.immutable.ListMap

class TreeLikeSpec extends munit.FunSuite {
  test("TreeLike") {

    test("prepend and append correctly") {
      val expectedError: ErrorTree = ListMap(
        NonEmptyList.one(Nil) -> NonEmptyList.one(ErrorMsg("bah!"))
      )

      assertEquals(
        expectedError.prefixWith("test").atPath(List("test")),
        expectedError
      )
    }

    test("retain order when composed") {

      assertEquals(
        ListMap(1 -> "one") |+| ListMap(2 -> "two"),
        ListMap(1 -> "one", 2 -> "two")
      )

      val orderedError1: ErrorTree = ListMap(
        NonEmptyList.one(List("one")) -> NonEmptyList.one(ErrorMsg("bah!"))
      )

      val orderedError2: ErrorTree = ListMap(
        NonEmptyList.one(List("a")) -> NonEmptyList.one(ErrorMsg("bah!"))
      )

      val combined = orderedError1 |+| orderedError2
      assertEquals(combined.head._1, NonEmptyList.one(List("one")))

    }
  }

}
