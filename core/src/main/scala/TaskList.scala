package ltbs.uniform

import org.atnos.eff._

case class TaskList2[A,AS,B,BS,STACK](
  a: (String, Eff[AS,A]),
  b: (String, Eff[BS,B])
)(
  implicit fa: IntoPoly[AS,STACK],
  fb: IntoPoly[BS,STACK]
)

object TaskList {
  def apply[A,AS,B,BS,STACK](
    a: (String, Eff[AS,A]),
    b: (String, Eff[BS,B])
  )(
    implicit fa: IntoPoly[AS,STACK],
    fb: IntoPoly[BS,STACK]
  ) = TaskList2(a,b)
}
