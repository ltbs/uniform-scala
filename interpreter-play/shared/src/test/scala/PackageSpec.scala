package ltbs.uniform
package interpreters.playframework

import org.scalatest._
import cats.implicits._

class PackageSpec extends FlatSpec with Matchers {

  "relativePath" should "move to sibling correctly" in {
    relativePath(List("one"), List("two")) shouldBe ("two")
  }

  it should "move up correctly" in {
    relativePath(List("parentOne", "one"), List("parentTwo", "two")) shouldBe ("../parentTwo/two")
  }

}
