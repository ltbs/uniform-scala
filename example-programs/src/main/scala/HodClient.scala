package ltbs.uniform
package examples

import scala.language.higherKinds

import cats.Monad
import cats.implicits._

case class User(
  forename: String,
  surname: String,
  age: Int
)

trait HodLanguage[F[_]] {
  def lookupUser(username: String): F[Option[User]]
}

object HodClient {

  //TODO: Adapters for new language

  // def beardTax(
  //   hod: HodLanguage[F]
  // ) = {
  //   import interpreter._
  //   for {
  //     userName      <- ask[String]("username")
  //     userOpt       <- hod.lookupUser(userName)
  //     _             <- userOpt match {
  //       case Some(user) => tell("found-user", user)
  //       case None       => tell("no-user-found", "No user found!")
  //     }
  //   } yield userOpt
  // }

  // type AUAskTypes = String :: Int :: NilTypes
  // def addUser[F[_] : cats.Applicative](
  //   interpreter: Language[F, NilTypes, AUAskTypes]
  // ): F[User] = {
  //   import interpreter._
  //   (
  //     ask[String]("forename"), 
  //     ask[String]("surname"),
  //     ask[Int]("age")
  //   ).mapN(User)
  // }

}
