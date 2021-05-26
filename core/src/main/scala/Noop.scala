package ltbs.uniform

trait Noop[A]

object Noop{
  implicit def noop[A]: Noop[A] = new Noop[A] {}
}

