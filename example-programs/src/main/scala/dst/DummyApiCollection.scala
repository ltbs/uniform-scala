package ltbs.uniform.examples.dst

import scala.language.higherKinds

import cats.implicits._
import cats.{Id, ~>}
import apis._
import java.time.{LocalDate, LocalDateTime}
import cats.data.NonEmptySet

object DummyAuth extends auth.AuthInterface[Id] {

  import apis.auth._

  val authRecord = AuthRecord(
    UkAddress("12 The Street", None, None, None, "AB12 3CD"),
    apis.Nino("ABC123456")
  )
  def forceLogin: AuthRecord = authRecord
  def getLoginDetails: Option[AuthRecord] =
    authRecord.some

  def convert[G[_]](nat: Id ~> G) = {
    val orig = this
    new AuthInterface[G] {
      def forceLogin: G[AuthRecord] = nat(orig.forceLogin)
      def getLoginDetails: G[Option[AuthRecord]] =
        nat(orig.getLoginDetails)
    }
  }
}

object DummySubscriber extends customer.EeittCustomerSubscription[Id] {
  import customer._
  def apply(
    regime: String,
    identification: apis.Identification,
    registration: RegistrationDetails,
    entity: Map[String,String],
    premise: Map[String,String],
    agent: Map[String,String]
  ): Either[NonEmptySet[ErrorResponse],EeittCustomerSubscriptionResponse] =
    EeittCustomerSubscriptionResponse(LocalDateTime.now, "1234").asRight[NonEmptySet[ErrorResponse]]

  def convert[G[_]](nat: Id ~> G) = {
    val orig = this
    new customer.EeittCustomerSubscription[G] {
      def apply(
        regime: String,
        identification: apis.Identification,
        registration: RegistrationDetails,
        entity: Map[String,String],
        premise: Map[String,String],
        agent: Map[String,String]
      ) = nat(orig.apply(regime, identification, registration, entity, premise, agent))
    }
  }

}

object DummyEeittReturn extends eeittreturn.EeittReturn[Id] {
  import eeittreturn._
  def apply(
    regime: String,
    identification: apis.Identification,
    periodKey: String,
    period: (LocalDate, LocalDate),
    regimeSpecificDetails: Map[String,String],
    receivedAt: LocalDateTime
  ): Either[NonEmptySet[ErrorResponse],EeittReturnResponse] =
    EeittReturnResponse(LocalDateTime.now, "1234").asRight[NonEmptySet[ErrorResponse]]

  def convert[G[_]](nat: Id ~> G) = {
    val orig = this
    new eeittreturn.EeittReturn[G] {
      def apply(
        regime: String,
        identification: apis.Identification,
        periodKey: String,
        period: (LocalDate, LocalDate),
        regimeSpecificDetails: Map[String,String],
        receivedAt: LocalDateTime
      ) = nat(orig.apply(regime, identification, periodKey, period, regimeSpecificDetails, receivedAt))
    }
  }
}

object DummyGetObligation extends getobligation.GetObligation[Id] {
  import apis.getobligation._
  def apply(
    identification: apis.Identification,
    regimeType: String,
    status: Set[apis.getobligation.Status],
    range: (LocalDate, LocalDate)
  ): Either[NonEmptySet[ErrorResponse],List[Obligation]] =
    List.empty[Obligation].asRight[NonEmptySet[ErrorResponse]]

  def convert[G[_]](nat: Id ~> G) = {
    val orig = this
    new getobligation.GetObligation[G] {
      def apply(
        identification: apis.Identification,
        regimeType: String,
        status: Set[apis.getobligation.Status],
        range: (LocalDate, LocalDate)
      ) = nat(orig.apply(identification, regimeType, status, range))
    }
  }

}
