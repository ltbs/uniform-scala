package ltbs.uniform.examples.dst.apis
package customer

import scala.language.higherKinds
import java.time.{LocalDate => Day, LocalTime}
import enumeratum._
import cats.data.NonEmptySet

sealed trait ErrorResponseCode extends EnumEntry

trait Identification

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


sealed trait LegalEntity extends EnumEntry

object LegalEntity extends Enum[LegalEntity] {
  def values = findValues
 
  case object SoleProprietor extends LegalEntity
  case object LLP extends LegalEntity
  case object Partnership extends LegalEntity
  case object UnincorporatedBody extends LegalEntity
  case object Trust extends LegalEntity
  case object LimitedCompany extends LegalEntity
  case object LloydsSyndicate extends LegalEntity
}

sealed trait Address {
  def line1: String
  def line2: Option[String]
  def line3: Option[String]
  def line4: Option[String]
  def countryCode: String
  def postalCodeOpt: Option[String]
  def lines: List[String] =
    {line1 :: List(line2, line3, line4, postalCodeOpt).flatten} :+ countryCode
}

case class UkAddress(
  line1: String,
  line2: Option[String], // "^[A-Za-z0-9 \\-,.&']{1,35}$"
  line3: Option[String], // "^[A-Za-z0-9 \\-,.&']{1,35}$"
  line4: Option[String], // "^[A-Za-z0-9 \\-,.&']{1,35}$"
  postalCode: String // "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}|BFPO\\s?[0-9]{1,10}$"
) extends Address {
  def countryCode: String = "GB"
  def postalCodeOpt = Some(postalCode)
}

case class ForeignAddress(
  line1: String,
  line2: Option[String], // "^[A-Za-z0-9 \\-,.&']{1,35}$"
  line3: Option[String], // "^[A-Za-z0-9 \\-,.&']{1,35}$"
  line4: Option[String], // "^[A-Za-z0-9 \\-,.&']{1,35}$"
  postalCodeOpt: Option[String], // "^[A-Z]{1,2}[0-9][0-9A-Z]?\\s?[0-9][A-Z]{2}|BFPO\\s?[0-9]{1,10}$"
  countryCode: String
) extends Address

case class ContactDetails(
  manualAddressMode: Boolean,
  address: Address,
  telephoneNumber: String,
  mobileNumber: String,
  email: String,
  faxNumber: String
)

sealed trait Title extends EnumEntry

object Title extends Enum[Title] {
  def values = findValues
  case object Mr extends Title
  case object Mrs extends Title
  case object Miss extends Title
  case object Ms extends Title
  case object Dr extends Title
  case object Sir extends Title
  case object Rev extends Title
  case object PersonalRepresentative extends Title
  case object Professor extends Title
  case object Lord extends Title
  case object Lady extends Title
  case object Dame extends Title
}

case class CustomerIdNumber(
  custIDNumber: String, // 0-15 chars
  noIdentifier: Boolean = false,
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
  title: Boolean,
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
  isrScenario: String, // ???
  commonDetails: CommonDetails,
  formData: Map[String, String]
)

case class Entity(
  isrScenario: Option[String], // ???
  formData: Map[String, String]
)

case class Premise(
  isrScenario: Option[String], // ???
  formData: Map[String, String]
)

case class EeittCustomerSubscriptionResponse (
  processingDate: LocalTime,
  formBundleNumber: String
)

trait EeittCustomerSubscription[F[_]] {
  def apply(
    regime: String, // ^(AGL|LFT|APD|IPT|BD|GD|ZBFP|ZGRF|ZFD|DST)$
    identification: Identification, 
    registration: RegistrationDetails,
    entity: Option[Entity],
    premise: Option[Premise],
    agent: Map[String, String]
  ): F[Either[NonEmptySet[ErrorResponse], EeittCustomerSubscriptionResponse]]
}
