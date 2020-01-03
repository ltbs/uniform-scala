package ltbs.uniform
package examples.witchcraft

sealed trait Gender

object Gender {
  case object FemaleWitch extends Gender
  case object MaleWarlock extends Gender
  case object Shapeshifting extends Gender
}

case class Accused (
  name: String,
  age: Int,
  gender: Gender,
  appearance: String
)
