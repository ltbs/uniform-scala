package ltbs.uniform.examples.dst.apis
package paymenttransactions

import scala.language.higherKinds
import java.time.{LocalDate => Day, LocalTime}
import enumeratum._
import cats.data.NonEmptySet

sealed trait ErrorResponseCode extends EnumEntry

object ErrorResponseCode extends Enum[ErrorResponseCode] {
  def values = findValues

  case object BadRequest extends ErrorResponseCode
  case object InvalidPayload extends ErrorResponseCode
  case object ServerError extends ErrorResponseCode
  case object ServiceUnavailable extends ErrorResponseCode
}

case class ErrorResponse(
  code: ErrorResponseCode,
  reason: String
)

case class Money(pence: Int)

/* TODO: This can pretty much all be generated */
case class Header(
  messageSequence: Int,
  numberOfRecords: Int, // 1 to 2000
  totalNet: Money, 
  totalCommission: Money,
  totalGross: Money,
  messageGeneratedOn: LocalTime
)

sealed trait PaymentMethod extends EnumEntry

object PaymentMethod extends Enum[PaymentMethod] {
  def values = findValues
  case object DebitCard  extends PaymentMethod
  case object CreditCard extends PaymentMethod
}

case class Transaction(
  paymentOn: Day,
  taxType: String,  // "Values can be P800, P302, PARC, PNGR, CDSX, NIIV or ZDST."

  /* Description from DES Spec - 
   * 
   * Value and Example:NINO ( including check character) P800 and start
   * of tax year eg. YN464834DP8002016 (17 Character long). NINO (
   * including check character) P302 and start of tax year
   * eg. YN464834DP3022016 (17 Character long). For PNGR the format
   * will be 14 character charge reference number eg. XaPRnnnnnnnnnn
   * (14 Character long). For PARC the format will be the standard
   * ETMP 14 character charge reference format eg. Xmnnnnnnnnnnnn
   * (where m is a modulus check)For BSP NI this will always be the
   * ‘NI’ identifier eg.XQNI00000100046 (15 Character long).Value and
   * Example:NINO ( including check character) P800 and start of tax
   * year eg. YN464834DP8002016 (17 Character long). NINO ( including
   * check character) P302 and start of tax year eg. YN464834DP3022016
   * (17 Character long). For 04AW CDS this will be CDSI + 12 char
   * charge reference - total 16 char long. For 84GN DST - this will
   * always be the DST identifier that has been assigned as part of
   * the subscription journey. No concatenations will be required eg
   * XADST0000010000 
   * 
   * "^(((?!(BG|GB|KN|NK|NT|TN|ZZ)|(D|F|I|Q|U|V)[A-Z]|[A-Z](D|F|I|O|Q|U|V))[A-Z]{2})[0-9]{6}[A-D]?P800\\d{4})$|^(((?!(BG|GB|KN|NK|NT|TN|ZZ)|(D|F|I|Q|U|V)[A-Z]|[A-Z](D|F|I|O|Q|U|V))[A-Z]{2})[0-9]{6}[A-D]?P302\\d{4})$|^(\\d{2}[A-Z][0-9]{5}P302[0-9]{4})$|^([A-Z]{4}[0-9]{10})$|^([A-Z]{4}[0-9]{11})$|^CDSI[a-zA-Z0-9]{12}$|^(X[A-Z]{1}[0-9]{12})$|^([A-Z]{2}DST[0-9]{10})$"
   */
  taxRef: String,
  paymentRef: String, // 1-64 chars,
  netAmount: Money,
  commission: Money,
  grossAmount: Money, //TODO: Calculate and move to def
  paymentMethod: PaymentMethod,
  /* ETMP Charge Reference: Example: XR007000003990. Mandatory when Tax Type = ‘NIIV’. Eg.XA000010177911. */
  chargeReference: Option[String], // "^[a-zA-Z0-9]{14}$"   
)

trait PaymentTransactions[F[_]] {

  def apply(
    header: Header,
    transactions: List[Transaction]
  ): F[Either[NonEmptySet[ErrorResponse], Unit]]
}
