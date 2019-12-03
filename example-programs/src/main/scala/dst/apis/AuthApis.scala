package ltbs.uniform.examples.dst
package apis
package auth

import scala.language.higherKinds

case class AuthRecord(
  address: Address,
  id: Identification
)

trait AuthInterface[F[_]] {
  def getLoginDetails: F[Option[AuthRecord]]
  def forceLogin: F[AuthRecord]
}
