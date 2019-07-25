package ltbs.uniform
package examples.beardtax

case class MemberOfPublic(
  forename: NonEmptyString,
  surname: NonEmptyString,
  age: java.time.LocalDate
)
