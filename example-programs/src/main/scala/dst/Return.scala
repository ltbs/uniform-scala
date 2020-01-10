package ltbs.uniform
package examples.dst

case class Return(
  alternateCharge: Map[Activity, Int],
  crossBorderReliefAmount: Long,
  companiesAmount: Map[GroupCompany, Long],
  allowanceAmount: Long, 
  totalLiability: Long,
  repayment: Option[RepaymentDetails]
)
