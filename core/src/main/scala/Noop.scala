package ltbs.uniform

trait Noop[A]

object Noop{
  implicit def noop[A] = new Noop[A] {}
}
