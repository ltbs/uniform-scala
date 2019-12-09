package ltbs.uniform.examples.dst.apis
package registration

import scala.language.higherKinds
import java.time.{LocalDate => Day}
import enumeratum._
import cats.data.NonEmptySet

sealed trait Identification

case class NationalInsuranceNumber (
  value: String
) extends Identification

case class UniqueTaxpayerReference(
  value: String
) extends Identification

case class EconomicOperatorRegistrationId (
  value: String
) extends Identification

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

sealed trait OrganisationType extends EnumEntry

object OrganisationType extends Enum[OrganisationType] {

  def values = findValues

  case object Partnership        extends OrganisationType
  case object LLP                extends OrganisationType
  case object CorporateBody      extends OrganisationType
  case object UnincorporatedBody extends OrganisationType
}

sealed trait ErrorCode extends EnumEntry

object ErrorCode extends Enum[ErrorCode] {

  def values = findValues

  case object InvalidUtr         extends ErrorCode
  case object InvalidNino        extends ErrorCode
  case object InvalidEori        extends ErrorCode
  case object InvalidTrn         extends ErrorCode
  case object InvalidIdtype      extends ErrorCode
  case object InvalidPayload     extends ErrorCode
  case object MalformedPayload   extends ErrorCode
  case object Conflict           extends ErrorCode
  case object NotFound           extends ErrorCode
  case object ServerError        extends ErrorCode
  case object ServiceUnavailable extends ErrorCode
}

case class ErrorResponse (
  code: ErrorCode,
  reason: String
)

sealed trait Entity

case class Organisation (
  name: String, 
  orgType: Option[OrganisationType]
) extends Entity

case class Individual (
  forename: String,
  surname: String, 
  dateOfBirth: Option[Day]
) extends Entity

case class ContactDetails (
  primaryPhone: Option[String], // "^[A-Z0-9 )/(\\-*#+]+$"
  secondaryPhone: Option[String], // "^[A-Z0-9 )/(\\-*#+]+$"
  faxNumber: Option[String], //"^[A-Z0-9 )/(\\-*#+]+$"
  emailAddress: Option[String]
)

case class RegistrationResponse(
  safeId: String, // "^X[A-Z]000[0-9]{10}$"
  sapNumber: Option[String], // 10 chars
  agentReferenceNumber: Option[String], // "^[A-Z](ARN)[0-9]{7}$"
  isEditable: Boolean,
  isAnAgent: Boolean,
  isAnASAgent: Boolean,
  isAnIndividual: Boolean,
  entity: Entity,
  address: Address,
  contactDetails: ContactDetails
)

trait Registration[F[_]] {
  def fetchOrganisation(
    id: Identification,
    regime: String,
    isAnAgent: Boolean,
    name: String,
    orgType: Option[OrganisationType]
  ): F[Either[NonEmptySet[ErrorResponse], Organisation]]

  def fetchIndividual(
    id: Identification,
    regime: String,
    isAnAgent: Boolean,
    forename: String,
    surname: String,
    dateOfBirth: Option[Day]
  ): F[Either[NonEmptySet[ErrorResponse], Individual]]

  /** I have literally no idea what this is for */
  def noIndividualOrOrganisation(
    regime: String,
    isAnAgent: Boolean
  ): F[Any]
}

