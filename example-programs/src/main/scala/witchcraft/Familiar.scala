package ltbs.uniform
package examples.witchcraft

sealed trait Familiar

object Familiar {
  case class Cat(name: String, isBlack: Boolean) extends Familiar
  case class Dog(glowingRedEyes: Boolean) extends Familiar
  case class Horse(pale: Boolean, skeletal: Boolean) extends Familiar
  case object Bird extends Familiar
}
