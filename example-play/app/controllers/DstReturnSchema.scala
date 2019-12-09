package controllers

import scala.language.higherKinds

import ltbs.uniform.examples.dst._
import cats.implicits._
import cats.{Id, ~>}
import apis._
import java.time.LocalDateTime
import cats.data.NonEmptySet
import play.api.libs.json._
import com.github.fge.jackson.JsonLoader
import com.github.fge.jsonschema.core.report.ProcessingReport
import com.github.fge.jsonschema.main.JsonSchemaFactory
import _root_.java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

case class SchemaChecker(schemaStream: java.io.InputStream) {
  val schemaText = scala.io.Source.fromInputStream(schemaStream).getLines.mkString
  schemaStream.close
  val schema = JsonLoader.fromString(schemaText)
  val validator = JsonSchemaFactory.byDefault.getValidator

  def errorsIn(json: JsValue): Option[ProcessingReport] = {
    val jsonIn = JsonLoader.fromString(Json.prettyPrint(json))
    val processingReport: ProcessingReport = validator.validate(schema, jsonIn)
    processingReport.some.filterNot(_.isSuccess)
  }
}

object SchemaChecker {
  implicit class RichJsValue[A <: JsValue](in: A){
    def checkAgainst(s: SchemaChecker): Either[ProcessingReport, A] = s.errorsIn(in) match {
      case Some(err) => err.asLeft
      case None => in.asRight
    }
  }
}

object DstReturnSchema extends eeittreturn.EeittReturn[Id] {
  import eeittreturn._

  lazy val schema = SchemaChecker(getClass.getResourceAsStream("/dst/returns.schema.json"))

  def apply(
    dstRegNo: String, 
    period: Period,
    activity: Map[Activity, ActivityReturn],
    repaymentDetails: Option[RepaymentDetails],
    finInfo: FinancialInformation,
    breakdown: List[LiabilityBreakdownEntry],
    isAmend: Boolean = false
  ): Either[NonEmptySet[ErrorResponse],EeittReturnResponse] = {

    def bool(in: Boolean): String = if(in) "X" else " "

    import Activity._
    val activityEntries: Seq[(String, String)] =
      activity.toList flatMap { case (activityType,v) =>

        val key = activityType match {
          case SocialMedia => "SOCIAL"
          case SearchEngine => "SEARCH"
          case Marketplace => "MARKET"
        }

        List(
          s"DST_${key}_CHARGE_PROVISION" -> bool(v.alternateChargeProvision),
          s"DST_${key}_LOSS" -> bool(v.loss),
          s"DST_${key}_OP_MARGIN" -> v.margin.toString
        )
      }


    val subjectEntries: Seq[(String, String)] =
      Activity.values.map{ activityType =>
        val key = activityType match {
          case SocialMedia => "SOCIAL"
          case SearchEngine => "SEARCHENGINE"
          case Marketplace => "MARKETPLACE"
        }

        (s"DST_SUBJECT_${key}" -> bool(activity.isDefinedAt(activityType)))
      }

    val repaymentInfo: Seq[(String, String)] =
      repaymentDetails.fold(Seq.empty[(String, String)]){ bank => Seq(
        "BANK_NON_UK" -> bool(!bank.isUkAccount), 
        "BANK_BSOC_NAME" -> bank.bankName, // Name of bank or building society CHAR40
        "BANK_SORT_CODE" -> bank.sortCode, // Branch sort code CHAR6
        "BANK_ACC_NO" -> bank.accountNumber, // Account number CHAR8
        "BANK_IBAN" -> bank.iban, // IBAN if non-UK bank account CHAR34
        "BANK_NAME" -> bank.bankName, // Name of account CHAR40
        "BUILDING_SOC_ROLE" -> bank.buildingSocietyRef // Building Society reference CHAR20
      )  }

    val breakdownEntries: Seq[(String, String)] = breakdown flatMap { e =>
      Seq(
        "DST_GROUP_MEMBER" -> e.memberName, // Group Member Company Name CHAR40
        "DST_GROUP_MEM_ID" -> e.utr, // Company registration reference number (UTR) CHAR40
        "DST_GROUP_MEM_LIABILITY" -> e.memberLiability.toString // DST liability amount per group member BETRW_KK
      )
    }

    val regimeSpecificDetails: Seq[(String, String)] = Seq(
      "REGISTRATION_NUMBER" -> dstRegNo, // MANDATORY ID Reference number ZGEN_FBP_REFERENCE
      "PERIOD_FROM" -> period.start.toString, // MANDATORY Period From  DATS
      "PERIOD_TO" -> period.start.toString, // MANDATORY Period To  DATS
      "DST_FIRST_RETURN" -> bool(isAmend), // Is this the first return you have submitted for this company and this accounting period? CHAR1
      "DST_RELIEF" -> finInfo.crossBorderRelief.toString, // Are you claiming relief for relevant cross-border transactions? CHAR1
      "DST_TAX_ALLOWANCE" -> finInfo.taxFreeAllowance.toString, // What tax-free allowance is being claimed against taxable revenues? BETRW_KK
      "DST_GROUP_LIABILITY" -> finInfo.totalLiability.toString, // MANDATORY Digital Services Group Total Liability BETRW_KK
      "DST_REPAYMENT_REQ" -> bool(repaymentDetails.isDefined), // Repayment for overpayment required? CHAR1
      "DATA_ORIGIN" -> "1" // MANDATORY Data origin CHAR2      
    ) ++ subjectEntries ++ activityEntries ++ repaymentInfo ++ breakdownEntries



    val regimeSpecificJson = JsArray(
      regimeSpecificDetails.zipWithIndex map { case ((key, value), i) => 
        Json.obj(
          "paramSequence" -> i.toString,
          "paramName" -> key,
          "paramValue" -> value
        )
      }
    )

    val jsonPayload = Json.obj(
      "receivedAt" -> ZonedDateTime.now().format(DateTimeFormatter.ISO_INSTANT),
      "periodFrom" -> period.start,
      "periodTo" -> period.end,
      "returnsDetails" -> Json.obj(
        "isrScenario" -> isrScenario,
        "regimeSpecificDetails" -> regimeSpecificJson
      )
    )

    println(Json.prettyPrint(jsonPayload))

    schema.errorsIn(jsonPayload).map{ report => 
      throw new IllegalStateException(report.toString)
    }

    EeittReturnResponse(LocalDateTime.now, "1234").asRight[NonEmptySet[ErrorResponse]]

  }

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
}
