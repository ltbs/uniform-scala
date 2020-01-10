package ltbs.uniform
package examples.dst

import scala.language.higherKinds
import cats.~>
trait BackendService[F[_]] {
  def matchedCompany(): F[Option[Company]]
  def lookup(utr: String, postcode: String): F[Option[Company]]

  def natTransform[G[_]](transform: F ~> G) = {
    val old = this
    new BackendService[G] {
      def matchedCompany(): G[Option[Company]] =
        transform(old.matchedCompany())
      def lookup(utr: String, postcode: String): G[Option[Company]] =
        transform(old.lookup(utr, postcode))
    }
  }
}
