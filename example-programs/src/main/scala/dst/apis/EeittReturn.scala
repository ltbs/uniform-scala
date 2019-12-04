package ltbs.uniform.examples.dst
package apis
package eeittreturn

import scala.language.higherKinds
import java.time.{LocalDate => Day, LocalDateTime}
import enumeratum._
import cats.data.NonEmptySet

sealed trait ErrorResponseCode extends EnumEntry

object ErrorResponseCode extends Enum[ErrorResponseCode] {
  def values = findValues

  case object InvalidRegime extends ErrorResponseCode
  case object InvalidIdtype extends ErrorResponseCode
  case object InvalidIdnumber extends ErrorResponseCode
  case object InvalidPayload extends ErrorResponseCode

  /** The back end has indicated that business partner key information cannot be found for the id number. */
  case object NotFoundBpkey extends ErrorResponseCode

  /** The back end has indicated that the taxpayer profile cannot be found for the ID. */
  case object NotFoundId extends ErrorResponseCode
  case object DuplicateSubmission extends ErrorResponseCode
  case object ServerError extends ErrorResponseCode
  case object ServiceUnavailable extends ErrorResponseCode
}

case class ErrorResponse(
  code: ErrorResponseCode,
  reason: String
)

case class EeittReturnResponse(
  processingDate: LocalDateTime,
  formBundleNumber: String // "^[0-9]{12}$"
)

trait EeittReturn[F[_]] {

  val isrScenario = "ZDS1"

  def apply(
    identification: Identification,
    period: Period,
    activity: Map[Activity, ActivityReturn]
  ): F[Either[NonEmptySet[ErrorResponse], EeittReturnResponse]]
}
