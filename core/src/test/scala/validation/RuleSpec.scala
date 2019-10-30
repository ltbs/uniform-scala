package ltbs.uniform
package validation

import org.scalatest._, funspec.AnyFunSpec, matchers.should.Matchers
import cats.data.Validated.Invalid
import cats.implicits._

class RuleSpec extends AnyFunSpec with Matchers with OptTCOps {

  describe("Rule.condAtPath") {
    it("should return an error at the correct path") {
      def alwaysFalse[A]: A => Boolean = {_ => false}
      val path = List("one", "two")
      val expectedError = ErrorMsg("bah!").toTree.prefixWithMany(path)
      val Invalid(result) = Rule.condAtPath(path.head, path.tail:_*)(alwaysFalse, "bah!")(())
      result shouldBe (expectedError)
    }

    it("should be sane") {
      Map(1 -> List('a')) |+| Map(1 -> List('b')) shouldBe (Map(1 -> List('a','b')))
    }

    {

      val input = ""
      val rulea = Rule.nonEmpty[String]()
      val ruleb = Rule.minLength(1)

      val Invalid(ruleaError) = rulea(input)
      val Invalid(rulebError) = ruleb(input)

      it("should run Lists of rules in parallel") {
        val rules: List[Rule[String]] = List(rulea, ruleb)
        val Invalid(combinedErrors) = rules.combineAll.apply(input)

        (ruleaError.valueAtRoot.get |+| rulebError.valueAtRoot.get) shouldBe (combinedErrors.valueAtRoot.get)
        combinedErrors.valueAtRoot.get.size shouldBe (2)
      }

      it("should run rules sequentially using andThen") {
        val Invalid(sequentialErrorsForward) = (rulea(input) andThen ruleb)
        val Invalid(sequentialErrorsBack)    = (ruleb(input) andThen rulea)
        sequentialErrorsForward shouldBe (ruleaError)
        sequentialErrorsBack    shouldBe (rulebError)        
      }
    }
  }
}
