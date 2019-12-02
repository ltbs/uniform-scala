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
  title : String,
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

case class Address (
  line1 : String,
  line2 : String,
  line3 : String,
  postcode : String,
  countryCode : String
)

case class GroupUltimateOwner (
  ownerName : String,
  companyRegNo : String,
  ownerAddress : Address
)

// ------- RETURNS

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
