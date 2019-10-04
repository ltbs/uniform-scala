package ltbs.uniform
package common.web

import org.scalatest._
import cats.implicits._

class RelativePathSpec extends FlatSpec with Matchers {

  "removeCommon" should "be correct for example cases" in {
    removeCommon(List("is-public", "aoeu"), List("is-public")) should be ((List("aoeu"),Nil))
    removeCommon(List("is-public"), List("is-public", "aoeu")) should be ((Nil, List("aoeu")))
    removeCommon(List("is-public"), List("beard-style")) should be ((List("is-public"), List("beard-style")))        
  }

  "relativePath" should "be correct for parent" in {
    relativePath(List("is-public", "aoeu"), List("is-public")) should be ("..")
  }

  it should "be correct for child" in {
    relativePath(List("is-public"), List("is-public", "aoeu")) should be ("is-public/aoeu")
  }

  it should "be correct for sibling" in {
    relativePath(List("is-public"), List("beard-style")) should be ("beard-style")
  }
}
