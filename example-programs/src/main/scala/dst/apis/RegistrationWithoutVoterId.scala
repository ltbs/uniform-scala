package ltbs.uniform.examples.dst.apis
package novoterid

import scala.language.higherKinds
import java.time.{LocalDate => Day}
import cats.data.NonEmptySet
import enumeratum._

sealed trait Entity

case class Organisation (
  name: String  
) extends Entity

case class Individual (
  firstName: String,
  middleName: Option[String], 
  lastName: String,
  dateOfBirth: Option[Day] 
) extends Entity

case class Identification (
  // Non-UK ID Number
  idNumber: String,
  issuingInstitution: String, // "^[a-zA-Z0-9 '&\\-\\/]{1,40}$"
  issuingCountryCode: String,  // "(?!^GB$)^[A-Z]{2}$"
)

case class ContactDetails (
  phoneNumber: Option[String], // "^[A-Z0-9 )/(\\-*#+]+$"
  mobileNumber: Option[String], // "^[A-Z0-9 )/(\\-*#+]+$"
  faxNumber: Option[String], //"^[A-Z0-9 )/(\\-*#+]+$"
  emailAddress: Option[String]
)

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

case class SuccessResponse(
  processingDate: Day,
  sapNumber: String, // 10 chars
  safeId: String, // "^X[A-Z]000[0-9]{10}$"
  agentReferenceNumber: Option[String] // "^[A-Z]ARN[0-9]{7}$"
)

sealed trait ErrorResponseCode extends EnumEntry

object ErrorResponseCode extends Enum[ErrorResponseCode] {
  def values = findValues
  case object InvalidPayload     extends ErrorResponseCode
  case object InvalidSubmission  extends ErrorResponseCode
  case object NotFound           extends ErrorResponseCode
  case object ServerError        extends ErrorResponseCode
  case object ServiceUnavailable extends ErrorResponseCode
}

case class ErrorResponse(
  code: ErrorResponseCode,
  reason: String
)

trait RegistrationWithoutVoterId[F[_]] {

  def register(
    //The type of Tax Regime. Values for regime: ATED, AWRS, AMLS, CDS, DDS, TAVC, ITSA, FHDDS, PODA, AGSV, CGT, DST",
    //pattern": 
    regime: String,  // "^[A-Z]{3,10}$"
    acknowledgementReference: String, // "^[A-Za-z0-9 -]{1,32}$"
    isAnAgent: Boolean,
    isAGroup: Boolean,
    identification: Option[Identification], 
    entity: Entity, 
    address: Address, 
    contactDetails: ContactDetails
  ): F[Either[NonEmptySet[ErrorResponse], SuccessResponse]]

}

