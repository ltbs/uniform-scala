package ltbs.uniform

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import cats.data.NonEmptyList

class TreeLikeSpec extends AnyFlatSpec with Matchers {
  "TreeLike" should "do stuff" in {

    val expectedError: ErrorTree = collection.immutable.ListMap(NonEmptyList.one(Nil) -> NonEmptyList.one(ErrorMsg("bah!")))


    expectedError.prefixWith("test").atPath(List("test")) shouldBe (expectedError)
  }

}
