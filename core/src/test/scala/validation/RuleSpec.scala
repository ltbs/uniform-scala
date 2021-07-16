package ltbs.uniform
package validation

import cats.data.Validated.Invalid
import cats.implicits._

class RuleSpec extends munit.FunSuite {

  test("Rule.condAtPath") {
    test("should return an error at the correct path") {
      def alwaysFalse[A]: A => Boolean = {_ => false}
      val path = List("one", "two")
      val expectedError = ErrorMsg("bah!").toTree.prefixWithMany(path)
      val Invalid(result) = Rule.condAtPath(path.head, path.tail:_*)(alwaysFalse, "bah!")(())
      assertEquals(result, expectedError)
    }

    {
      val input = ""
      val rulea = Rule.nonEmpty[String]
      val ruleb = Rule.minLength[String](1)

      val Invalid(ruleaError) = rulea(input)
      val Invalid(rulebError) = ruleb(input)

      test("should run Lists of rules in parallel") {
        val rules: List[Rule[String]] = List(rulea, ruleb)
        val Invalid(combinedErrors) = rules.combineAll.apply(input)

        assertEquals((ruleaError.valueAtRoot.get |+| rulebError.valueAtRoot.get), combinedErrors.valueAtRoot.get)
        assertEquals(combinedErrors.valueAtRoot.get.size, 2)
      }

      test("should run rules sequentially using followedBy") {
        val Invalid(sequentialErrorsForward) = (rulea followedBy ruleb)(input)
        val Invalid(sequentialErrorsBack)    = (ruleb followedBy rulea)(input)
        assertEquals(sequentialErrorsForward, ruleaError)
        assertEquals(sequentialErrorsBack   , rulebError)
      }
    }
  }
}
