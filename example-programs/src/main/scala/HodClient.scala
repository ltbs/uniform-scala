package ltbs.uniform

package examples

import scala.language.higherKinds
import izumi.reflect.TagK

case class User(
  forename: String,
  surname: String,
  age: Int
)

trait HodLanguage[F[_]] {
  def lookupUser(username: String): F[Option[User]]
}

object HodClient {

  def beardTax[F[_]: TagK](
    hod: HodLanguage[F]
  ) = for {
    userName      <- ask[String]("username")
    userOpt       <- convert(hod.lookupUser(userName))
    _             <- userOpt match {
      case Some(user) => tell("found-user", user)
      case None       => tell("no-user-found", "No user found!")
    }
  } yield userOpt

  // def addUser = (
  //   ask[String]("forename"),
  //   ask[String]("surname"),
  //   ask[Int]("age")
  // ).mapN(User)

}
