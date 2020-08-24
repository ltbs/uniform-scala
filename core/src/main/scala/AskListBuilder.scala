package ltbs.uniform

import validation.Rule
import izumi.reflect.Tag

case class AskListBuilder[A] private[uniform] (key: String, default: Option[List[A]], validation: Rule[List[A]]) {
  def apply[R <: Needs[_]](
    base: (Option[Int], List[A]) => Uniform[R, A, Unit]
  )(
    implicit tag: Tag[A]
  ) = Uniform.ListOf[R, A](key, base, default, validation, Map.empty, tag)
}
