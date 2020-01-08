package ltbs.uniform
package examples
package beardtax

case class MemberOfPublic(
  forename: String,
  surname: String,
  age: java.time.LocalDate
) extends LooselyRelated
