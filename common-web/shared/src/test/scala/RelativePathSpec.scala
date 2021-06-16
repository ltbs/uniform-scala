package ltbs.uniform
package common.web

import cats.implicits._

class RelativePathSpec extends munit.FunSuite {

  test("removeCommon should be correct for example cases") {
    assertEquals(
      removeCommon(List("is-public", "aoeu"), (List("is-public"))),
      (List("aoeu"), Nil)
    )
    assertEquals(
      removeCommon(List("is-public"), List("is-public", "aoeu")),
      (Nil, List("aoeu"))
    )
    assertEquals(
      removeCommon(List("is-public"), List("beard-style")),
      (List("is-public"), List("beard-style"))
    )
  }

  test("relativePath") {
    test ("should be correct for parent") {
      assertEquals(relativePath(List("is-public", "aoeu"), List("is-public")), "..")
    }

    test("should be correct for child") {
      assertEquals(relativePath(List("is-public"), List("is-public", "aoeu")), "is-public/aoeu")
    }

    test("should be correct for sibling") {
      assertEquals(relativePath(List("is-public"), List("beard-style")), "beard-style")
    }

    test("should be correct for example case") {
      assertEquals(relativePath(List("evidence", "add", "add"), List("evidence")), "../..")
    }
  }
}
