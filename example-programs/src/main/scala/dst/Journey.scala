package ltbs.uniform
package examples

import scala.language.higherKinds

import cats.Monad
import cats.implicits._
import java.time.{LocalDate => Day}

package object dst {

  type Percent = Int
  type Money = Int

  type TellTypes = NilTypes
  type AskTypes = CustomerIdentification :: ContactDetails :: PrimaryPerson :: GroupUltimateOwner :: (Day,Day) :: NilTypes

  def registrationJourney[F[_] : Monad](
    i: Language[F, TellTypes, AskTypes]
  ): F[Registration] = {
    import i._
{
    (
      Day.now.pure[F],
      ask[CustomerIdentification]("customer-identification"),
      ask[ContactDetails]("contact-details"),
      ask[PrimaryPerson]("primary-person"),
      ask[GroupUltimateOwner]("group-ultimate-owner"),
      ask[(Day, Day)]("period")
    ) mapN Registration
  } >>= { reg: Registration => reg.pure[F] }

  }

  type AskTypesReturn = Basic :: CompanyInformation :: Set[Activity] :: ActivityReturn :: FinancialInformation :: List[LiabilityBreakdownEntry] :: NilTypes

  def returnJourney[F[_] : Monad](
    i: Language[F, TellTypes, AskTypesReturn]
  ): F[ReturnForm] = {
    import i._

    (
      ask[Basic]("basic"),
      ask[CompanyInformation]("company-information"),
      {ask[Set[Activity]]("activity") >>= { _.map{ a => ask[ActivityReturn](s"activity-$a").map(x => a -> x)}.toList.sequence}}.map{_.toMap},
      ask[FinancialInformation]("financial-information"),
      ask[List[LiabilityBreakdownEntry]]("breakdown")
    ) mapN ReturnForm

  }
  
}
