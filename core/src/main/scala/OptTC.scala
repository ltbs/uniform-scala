package ltbs.uniform

import com.github.ghik.silencer.silent
import shapeless.LowPriority
import shapeless.tag, tag.{@@}

object OptTCOps extends OptTCOps

trait OptTCOps {
  trait OptTCTag
  type OptTC[A] = Option[A] @@ OptTCTag

  implicit def optTcNone[T](implicit @silent lp: LowPriority): OptTC[T] = tag[OptTCTag][Option[T]](None)
  implicit def optTcSome[T](implicit s: T): OptTC[T] = tag[OptTCTag][Option[T]](Some(s))
}
