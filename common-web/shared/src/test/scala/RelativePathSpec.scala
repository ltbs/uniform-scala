package ltbs.uniform
package common.web

import org.scalatest._
import cats.implicits._

class RelativePathSpec extends FlatSpec with Matchers {

  "removeCommon" should "be correct for example cases" in {
    removeCommon(List("is-public", "aoeu"), List("is-public")) should be ((List("aoeu"),Nil))
  }

  "relativePath" should "be correct for parent" in {
    relativePath(List("is-public", "aoeu"), List("is-public")) should be ("..")
  }

  it should "be correct for child" in {
    relativePath(List("is-public"), List("is-public", "aoeu")) should be ("aoeu")
  }

  it should "be correct for sibling" in {
    relativePath(List("is-public"), List("beard-style")) should be ("../beard-style")
  }
}
