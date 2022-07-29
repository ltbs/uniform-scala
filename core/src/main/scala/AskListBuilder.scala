package ltbs.uniform

import validation.Rule
import izumi.reflect.Tag

case class AskListBuilder[A] private[uniform] (key: String, default: Option[List[A]], validation: Rule[List[A]]) {
  def apply[R <: Needs[_,_], T1, T2](
    base: (Option[Int], List[A]) => Uniform[R, T1, A],
    deleteConfirmation: (Int, List[A]) => Uniform[R, T2, Boolean] = {(_: Int, _ : List[A]) => pure(true)}
  )(
    implicit tag: Tag[A],
    tagt1: Tag[T1],
    tagt2: Tag[T2],
  ) = Uniform.ListOf[R, A, T1, T2](key, base, deleteConfirmation, default, validation, Map.empty, tag, tagt1, tagt2)
}
