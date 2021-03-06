package ltbs.uniform

import validation.Rule
import izumi.reflect.Tag

case class InteractBuilder[A] private[uniform] () {
  def apply[T: Tag](
    key: String,
    value: T,
    default: Option[A] = None,
    validation: Rule[A] = Rule.alwaysPass[A],
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(implicit taga: Tag[A]): Uniform[Needs.Interact[T, A], T, A] =
    Uniform.Interact(key, value, default, validation, customContent, implicitly[Tag[T]], implicitly[Tag[A]])
}
