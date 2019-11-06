package ltbs.uniform

import scala.language.higherKinds

import com.github.ghik.silencer.silent
import shapeless.LowPriority
import shapeless.tag, tag.{@@}

object OptTCSyntax extends OptTCOps

trait OptTCOps {

  trait OptTCTag
  type OptTC[A] = Option[A] @@ OptTCTag

  implicit def optTcNone[TC[_], T](implicit @silent lp: LowPriority): OptTC[TC[T]] =
    tag[OptTCTag][Option[TC[T]]](None)

  implicit def optTcSome[TC[_], T](implicit s: TC[T]): OptTC[TC[T]] =
    tag[OptTCTag][Option[TC[T]]](Some(s))
}
