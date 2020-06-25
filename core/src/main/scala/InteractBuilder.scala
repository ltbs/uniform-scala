package ltbs.uniform

import validation.Rule
import izumi.reflect.Tag

case class InteractBuilder[A] private[uniform] () {
  def apply[T: Tag](
    key: String,
    value: T,
    default: Option[A] = None,
    validation: Rule[A] = Rule.alwaysPass[A]
  )(implicit taga: Tag[A]): Uniform[Needs.Ask[A] with Needs.Tell[T], A, T] =
    Uniform.Interact(key, value, default, validation, Map.empty, implicitly[Tag[A]], implicitly[Tag[T]])
}
