package ltbs.uniform

import com.github.ghik.silencer.silent
import shapeless.LowPriority
import language.higherKinds

object OptTC extends OptTC

trait OptTC {
  implicit def optTcNone[TC[_], T](implicit @silent lp: LowPriority): Option[TC[T]] = None
  implicit def optTcSome[TC[_], T](implicit s: TC[T]): Option[TC[T]] = Some(s)
}
