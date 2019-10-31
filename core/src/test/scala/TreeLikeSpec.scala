package ltbs.uniform

import cats.data.NonEmptyList
import cats.implicits._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TreeLikeSpec extends AnyFlatSpec with Matchers {
  "TreeLike" should "do stuff" in {

    val expectedError: ErrorTree = collection.immutable.ListMap(NonEmptyList.one(Nil) -> NonEmptyList.one(ErrorMsg("bah!")))


    expectedError.prefixWith("test").atPath(List("test")) shouldBe (expectedError)


    val orderedError1: ErrorTree = collection.immutable.ListMap(
      NonEmptyList.one(List("one")) -> NonEmptyList.one(ErrorMsg("bah!")),
      NonEmptyList.one(List("two")) -> NonEmptyList.one(ErrorMsg("bah!")),
      NonEmptyList.one(List("three")) -> NonEmptyList.one(ErrorMsg("bah!")),
    )

    val orderedError2: ErrorTree = collection.immutable.ListMap(
      NonEmptyList.one(List("a")) -> NonEmptyList.one(ErrorMsg("bah!")),
      NonEmptyList.one(List("b")) -> NonEmptyList.one(ErrorMsg("bah!")),
      NonEmptyList.one(List("c")) -> NonEmptyList.one(ErrorMsg("bah!")),
    )

    val combined = orderedError1 |+| orderedError2

    combined.keys.head.head shouldBe List("one")

  }

}
