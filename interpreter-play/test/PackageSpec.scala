package ltbs.uniform
package interpreters.playframework

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers

class PackageSpec extends AnyFlatSpec with Matchers {

  "relativePath" should "move to sibling correctly" in {
    relativePath(List("one"), List("two")) shouldBe ("two")
  }

  it should "move up correctly" in {
    relativePath(List("parentOne", "one"), List("parentTwo", "two")) shouldBe ("../parentTwo/two")
  }

}
