package ltbs.uniform

class Needs[+A]

object Needs {
  trait Tell[T] extends Needs[T] {
    def tellMarker: T
  }
  trait Ask[T] extends Needs[T] {
    def askMarker: T
  }
}
