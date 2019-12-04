package ltbs.uniform
package examples
package dst

import java.time.{LocalDate => Day}
import enumeratum._

case class Registration(
  day: Day,
  customerIdentification: CustomerIdentification,
  contactDetails: ContactDetails,
  primaryPerson: PrimaryPerson,
  ultimateOwnwer: GroupUltimateOwner,
  period: (Day, Day)
)

case class CustomerIdentification (
  customerIdentificationNumber : Option[String],
  organisationName : String,
  title : Title,
  forename : String,
  surname : String,
  dateOfBirth : Day
)

case class ContactDetails (
  address : Address,
  contactPhone : String,
  contactEmail : String
)

case class PrimaryPerson (
  name : String,
  primaryPhone : String,
  primaryEmail : String
)

case class GroupUltimateOwner (
  ownerName : String,
  companyRegNo : String,
  ownerAddress : Address
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

// ------- RETURNS

case class Period(
  start: Day,
  end: Day
)

case class Basic (
  businessPartner : String,
  contractAccount : String,
  threeThreeRt : String,
  formBundle : String,
  contractObject : String,
  registrationNumber : String,
  acknowledgement : String,
  period : (Day, Day),
  chargeRef : String
)

sealed trait Activity extends EnumEntry

object Activity extends Enum[Activity] { 

  val values = findValues

  case object SocialMedia extends Activity
  case object SearchEngine extends Activity
  case object Marketplace extends Activity
}

case class CompanyInformation (
  isAmendment : Boolean
)

case class ActivityReturn (
  alternateChargeProvision : Boolean,
  loss : Boolean,
  margin : Percent
)

object ActivityReturn {
  def empty = ActivityReturn(
    false, false, 0
  )
}

case class RepaymentDetails (
  isUkAccount : Boolean,
  bankName : String,
  sortCode : String,
  accountNumber : String,
  iban : String,
  accountName : String,
  buildingSocietyRef : String
)

case class FinancialInformation (
  crossBorderRelief : Boolean,
  taxFreeAllowance : Money,
  totalLiability : Money,
  repaymentDetails : Option[RepaymentDetails]
)

case class LiabilityBreakdownEntry (
  memberName : String,
  utr : String,
  memberLiability : Money
)

case class ReturnForm (
  basic: Basic,
  companyInformation: CompanyInformation,
  activity: Map[Activity, ActivityReturn],
  financialInformation: FinancialInformation,
  breakdown: List[LiabilityBreakdownEntry]
)
