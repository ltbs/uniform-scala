package ltbs.uniform.examples.dst
package apis
package customer

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

sealed trait DeclarationStatus extends EnumEntry

object DeclarationStatus extends Enum[DeclarationStatus] {
  def values = findValues
  case object AuthorisedOfficial extends DeclarationStatus
  case object CompanySecretary extends DeclarationStatus
  case object Director extends DeclarationStatus
  case object Partner extends DeclarationStatus
  case object SoleProprietor extends DeclarationStatus
  case object Trustee extends DeclarationStatus
  case object Other extends DeclarationStatus
}

case class LegacyCustomerReference (
  migratedFromLegacySystem: Boolean,
  referenceNumber: String // 1-30 chars
)

case class ContactDetails(
  manualAddressMode: Boolean,
  address: Address,
  telephoneNumber: String,
  mobileNumber: Option[String],
  email: String,
  faxNumber: Option[String]
)

case class CustomerIdNumber(
  custIDNumber: Option[String], // 0-15 chars
  organisationName: String, //1-160 chars
  title: Title,
  firstName: String,
  lastName: String,
  dateOfBirth: Day,
  dataMismatch: Boolean
)

case class AboutBusiness(
  organisationName: String,
  /* This one is odd, DES description is - 'No Custoemr [sic]
   * Identifier Indicator. 1: True, 0: False. Expected to always be
   * False for MDTP submissions'.
   */
  title: Title,
  firstName: String,
  lastName: String, 
  dateOfBirth: Day,
  tradingName: String
)

case class CommonDetails(
  legalEntity: LegalEntity,
  customerIdNumber: CustomerIdNumber,
  aboutBusiness: AboutBusiness,
  businessContact: ContactDetails,
  alternativeContact: Option[ContactDetails],
  legacyCustomerReference: LegacyCustomerReference,
  declarationStatus: DeclarationStatus,
  declarationAccepted: Boolean
)

case class RegistrationDetails (
  commonDetails: CommonDetails,
  formData: Map[String, String]
)

case class EeittCustomerSubscriptionResponse (
  processingDate: LocalDateTime,
  formBundleNumber: String
)

trait EeittCustomerSubscription[F[_]] {

  val isrScenario = "ZDS2"

  def apply(
    regime: String, // ^(AGL|LFT|APD|IPT|BD|GD|ZBFP|ZGRF|ZFD|DST)$
    identification: Identification, 
    registration: RegistrationDetails,
    entity: Map[String, String],
    premise: Map[String, String],
    agent: Map[String, String]
  ): F[Either[NonEmptySet[ErrorResponse], EeittCustomerSubscriptionResponse]]
}
