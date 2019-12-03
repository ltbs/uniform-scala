package ltbs.uniform
package examples

import scala.language.higherKinds

import cats.Monad
import cats.implicits._
import java.time.{LocalDate => Day}
import dst.apis._, getobligation._, customer.DeclarationStatus

package object dst {

  type Percent = Int
  type Money = Int

  type TellTypes = NilTypes
  type AskTypes = LegalEntity :: DeclarationStatus :: CustomerIdentification :: ContactDetails :: PrimaryPerson :: GroupUltimateOwner :: (Day,Day) :: NilTypes

  def registrationJourney[F[_] : Monad](
    i: Language[F, TellTypes, AskTypes],
    gg: auth.AuthInterface[F],
    createSubscription: customer.EeittCustomerSubscription[F]
  ): F[Unit] = {
    import i._
    import customer.{ContactDetails => _, _}
    for {
      authRecord <- gg.forceLogin
      customer <- ask[CustomerIdentification]("customer-identification")
      legalEntity <- ask[LegalEntity]("legal-entity")
      contact <- ask[ContactDetails]("contact-details")
      declarationStatus <- ask[DeclarationStatus]("declaration-status")      
      primary <- ask[PrimaryPerson]("primary-person")
      ultimateOwner <- ask[GroupUltimateOwner]("group-ultimate-owner")
      period <- ask[(Day, Day)]("period")
    } yield (
      createSubscription.apply(
        "DST",
        authRecord.id,
        RegistrationDetails (
          CommonDetails(
            legalEntity = legalEntity,
            customerIdNumber = CustomerIdNumber(
              custIDNumber = customer.customerIdentificationNumber, // 0-15 chars
              organisationName = customer.organisationName, //1-160 chars
              title = customer.title,
              firstName = customer.forename,
              lastName = customer.surname,
              dateOfBirth = customer.dateOfBirth,
              dataMismatch = false
            ),
            aboutBusiness = AboutBusiness(
              organisationName = customer.organisationName,
              title = customer.title,
              firstName = customer.forename,
              lastName = customer.surname,
              dateOfBirth = customer.dateOfBirth,
              tradingName = customer.organisationName
            ),
            businessContact = dst.apis.customer.ContactDetails(
              manualAddressMode = true, //: Boolean,
              address = contact.address, //: Address,
              telephoneNumber = primary.primaryPhone, //: String,
              mobileNumber = None, //: String,
              email = primary.primaryEmail, //: String,
              faxNumber = None //: String
            ),
            alternativeContact = None, /// : Option[customer.ContactDetails],
            legacyCustomerReference = LegacyCustomerReference (
              migratedFromLegacySystem = false, //: Boolean,
              referenceNumber = "" //: String // 1-30 chars
            ), 
            declarationStatus = declarationStatus, //: DeclarationStatus - who is making the declaration
            declarationAccepted = true // : Boolean - always true? or do we just omit this?
          ),
          formData = Map() //: Map[String, String]
        ),
        entity = Map(), //: Map[String, String]
        premise = Map(), //: Map[String, String]
        agent = Map() // : Map[String, String]
      )
    )

  }

  type AskTypesReturn = Basic :: CompanyInformation :: Set[Activity] :: ActivityReturn :: FinancialInformation :: List[LiabilityBreakdownEntry] :: NilTypes

  def returnJourney[F[_] : Monad](
    i: Language[F, TellTypes, AskTypesReturn],
    id: Identification, 
    eeittReturns: eeittreturn.EeittReturn[F],
    obligations: getobligation.GetObligation[F]
  ): F[Unit] = {
    import i._

    for {
      completedPeriods <- obligations.apply(id, "DST", Set(Status.Fulfilled), (Day.now.minusYears(2), Day.now))
      basic <- ask[Basic]("basic")
      company <- ask[CompanyInformation]("company-information")
      activity <- {ask[Set[Activity]]("activity") >>= { _.map{ a => ask[ActivityReturn](s"activity-$a").map(x => a -> x)}.toList.sequence}}.map{_.toMap}
      financial <- ask[FinancialInformation]("financial-information")
      breakdown <- ask[List[LiabilityBreakdownEntry]]("breakdown")
    } yield (eeittReturns.apply(
      "DST",
      id,
      "period",
      basic.period,
      Map() //     regimeSpecificDetails: Map[String, String],
    ))
  }  
}
