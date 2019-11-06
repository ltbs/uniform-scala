package ltbs.uniform

import cats.data.NonEmptyList
import cats.implicits._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import collection.immutable.ListMap

class TreeLikeSpec extends AnyFlatSpec with Matchers {
  "TreeLike" should "prepend and append correctly" in {

    val expectedError: ErrorTree = ListMap(
      NonEmptyList.one(Nil) -> NonEmptyList.one(ErrorMsg("bah!"))
    )

    expectedError.prefixWith("test").atPath(List("test")) shouldBe (expectedError)

  }

  it should "retain order when composed" in {

    {ListMap(1 -> "one") |+| ListMap(2 -> "two")} shouldBe ListMap(1 -> "one", 2 -> "two")

    val orderedError1: ErrorTree = ListMap(
      NonEmptyList.one(List("one")) -> NonEmptyList.one(ErrorMsg("bah!"))
    )

    val orderedError2: ErrorTree = ListMap(
      NonEmptyList.one(List("a")) -> NonEmptyList.one(ErrorMsg("bah!"))
    )

    val combined = orderedError1 |+| orderedError2
    combined.head._1 shouldBe NonEmptyList.one(List("one"))

  }

}
