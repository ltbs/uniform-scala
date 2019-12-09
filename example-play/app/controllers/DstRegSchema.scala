package controllers

import scala.language.higherKinds

import ltbs.uniform.examples.dst._
import cats.implicits._
import cats.{Id, ~>}
import apis._
import java.time.{LocalDateTime, LocalDate, ZonedDateTime}
import cats.data.NonEmptySet
import play.api.libs.json._
import com.github.fge.jackson.JsonLoader
import com.github.fge.jsonschema.core.report.ProcessingReport
import com.github.fge.jsonschema.main.JsonSchemaFactory
import java.time.format.DateTimeFormatter

object DstRegSchema /* extends customer.EeittCustomerSubscription[Id] */ {
  import customer._

  lazy val schema = SchemaChecker(getClass.getResourceAsStream("/dst/1479-eeitt-subscribe.request.schema.json"))

  def apply(
  ): Either[NonEmptySet[ErrorResponse],EeittCustomerSubscriptionResponse] = {

    def bool(in: Boolean): String = if(in) "X" else " "

    import Activity._

    val jsonPayload = Json.obj(
      "registrationDetails" -> Json.obj(
        "isrScenario" -> "ZDS2",
        "commonDetails" -> Json.obj(
          "legalEntity" -> Json.obj(
	    "organisationType" -> "1",
	    "dateOfApplication" -> LocalDate.now.toString,
	    "taxStartDate" -> LocalDate.now.toString
          ),
          "customerIdentificationNumber" -> Json.obj(
	    "custIDNumber" -> "???",
	    "noIdentifier" -> "0", // ???
	    "organisationName" -> "???",
	    "title" -> "0001", // ???
	    "custFirstName" -> "???",
	    "custLastName" -> "???",
	    "custDOB" -> "1980-01-01", // ???
	    "dataMismatchIndicator" -> "0" // ???
          ),
	  "aboutBusiness" -> Json.obj(
	    "organisationName" -> "AbtBizOrgName",
	    "title" -> "1",
	    "firstName" ->  "AbtBizFirstName",
	    "lastName" -> "AbtBizLastName",
	    "dateOfBirth" -> "1980-11-01",
	    "tradingName" -> "AbtBizTradingName"
	  )
        )
      ),
      "siteDetails" -> Json.obj(
        "formData" -> JsArray(
          List(
            Json.obj(
              "commonDetails" -> Json.obj(
                "action" -> "1",

              )
            )
          )
        )
      )
    )

    println(Json.prettyPrint(jsonPayload))

    schema.errorsIn(jsonPayload).map{ report => 
      throw new IllegalStateException(report.toString)
    }

    EeittCustomerSubscriptionResponse(LocalDateTime.now, "1234").asRight[NonEmptySet[ErrorResponse]]

  }

  /*
  def convert[G[_]](nat: Id ~> G) = {
    val orig = this
    new eeittreturn.EeittReturn[G] {
      def apply(
        dstRegNo: String,
        period: Period,
        activity: Map[Activity, ActivityReturn],
        repaymentDetails: Option[RepaymentDetails],
        finInfo: FinancialInformation,
        breakdown: List[LiabilityBreakdownEntry],
        isAmend: Boolean = false
      ) = nat(orig.apply(dstRegNo, period, activity, repaymentDetails, finInfo, breakdown, isAmend))
    }
  }
   */
}
