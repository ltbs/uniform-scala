package ltbs.uniform
package examples.dst

sealed trait BankAccount

case class ForeignBankAccount(iban: String) extends BankAccount
case class DomesticBankAccount(sortCode: String, accountNo: String, buildingSocietyNumber: Option[String]) extends BankAccount

case class RepaymentDetails(
  accountName: String,
  bankAccount: BankAccount
)
