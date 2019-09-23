package ltbs.uniform
package examples
package beardtax

case class MemberOfPublic(
  forename: NonEmptyString,
  surname: NonEmptyString,
  age: java.time.LocalDate
) extends LooselyRelated
