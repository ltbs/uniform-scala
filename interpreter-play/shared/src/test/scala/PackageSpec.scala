package ltbs.uniform
package interpreters.playframework

class PackageSpec extends munit.FunSuite {

  test("relativePath") {
    test("should move to sibling correctly") {
      assertEquals(relativePath(List("one"), List("two")),"two")
    }

    test("should move up correctly") {
      assertEquals(relativePath(List("parentOne", "one"), List("parentTwo", "two")), "../parentTwo/two")
    }
  }

}
