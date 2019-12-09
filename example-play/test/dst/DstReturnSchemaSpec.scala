package controllers

import ltbs.uniform.examples.dst._, apis._
import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import java.time.LocalDate

class DstReturnSchemaSpec extends AnyFlatSpec with Matchers {

  "A return API call" should "conform to the schema" in {
    DstReturnSchema(
      "BCD", //identification
      Period(LocalDate.of(2018,1,1), LocalDate.of(2019,12,31)), //period: (LocalDate, LocalDate),
      Map.empty,
      None,
      FinancialInformation(false, 1, 1),
      Nil,
      false
    )
  }
 

}



class DstRegSchemaSpec extends AnyFlatSpec with Matchers {

  "A return API call" should "conform to the schema" in {
    DstRegSchema(
    )
  }
 

}

