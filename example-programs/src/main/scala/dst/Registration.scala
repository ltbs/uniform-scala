package ltbs.uniform
package examples.dst

import java.time.LocalDate

case class Registration (
       company: Company,
       alternativeContact: Option[Address], 
       ultimateParent: Option[Company], 
       contact: ContactDetails, 
       dateLiable: LocalDate, 
       accountingPeriodEnd: LocalDate
)
