package ltbs.uniform.examples.dst.apis
package getobligation

import scala.language.higherKinds
import java.time.{LocalDate => Day}
import enumeratum._
import cats.data.NonEmptySet

sealed trait Status extends EnumEntry

object Status extends Enum[Status] {
  def values = findValues
  case object Open      extends Status
  case object Fulfilled extends Status  
}

sealed trait IncomeSourceType extends EnumEntry

object IncomeSourceType extends Enum[IncomeSourceType] {
  def values = findValues  
  case object ITSA extends IncomeSourceType
  case object ITSB extends IncomeSourceType
  case object ITSP extends IncomeSourceType
}

case class ObligationId(
  incomeSourceType: Option[IncomeSourceType],
  referenceNumber: String, // "^[A-Za-z0-9]{1,15}$"
  referenceType: String, // "^[A-Za-z]{1,6}$"
)

case class ObligationDetail (
  status: Status,
  inboundCorrespondenceFrom: Day,
  inboundCorrespondenceTo: Day,
  inboundCorrespondenceReceived: Option[Day],
  inboundCorrespondenceDue: Day,
  periodKey: String // 1-4 chars
)

case class Obligation(
  identification: ObligationId,
  details: List[ObligationDetail]
)

sealed trait ErrorResponseCode extends EnumEntry

object ErrorResponseCode extends Enum[ErrorResponseCode] {
  def values = findValues
  case object InvalidIdType extends ErrorResponseCode
  case object InvalidIdNumber extends ErrorResponseCode
  case object InvalidStatus extends ErrorResponseCode
  case object InvalidRegime extends ErrorResponseCode
  case object InvalidDateTo extends ErrorResponseCode
  case object InvalidDateFrom extends ErrorResponseCode
  case object InvalidDateRange extends ErrorResponseCode
  case object NotFoundBpKey extends ErrorResponseCode
  case object NotFound extends ErrorResponseCode
  case object ServerError extends ErrorResponseCode
  case object ServiceUnavailable extends ErrorResponseCode
}

case class ErrorResponse(
  code: ErrorResponseCode,
  reason: String
)

trait GetObligation[F[_]] {
  def apply(
    identification: Identification,
    regimeType: String = "DST",
    status: Set[Status] = Status.values.toSet, 
    range:(Day, Day)
  ): F[Either[NonEmptySet[ErrorResponse], List[Obligation]]]
}
