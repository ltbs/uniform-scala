package ltbs.uniform
package interpreters.logictable

trait SampleData[A] {
  def apply(key: String): List[A]
}

object SampleData {
  def instance[A](in: A*) = new SampleData[A] {
    override def apply(key: String): List[A] = in.toList
  }
}

trait TellRenderer[A] {
  def apply(key: String, value: A): List[String]
}

case class LTInteraction[T, A](
  tellRenderer: TellRenderer[T],
  askRenderer: SampleData[A]
)

object LTInteraction {
  implicit def auto[T,A](
    implicit tellRenderer: TellRenderer[T],
    askRenderer: SampleData[A]
  ): LTInteraction[T,A] = LTInteraction(tellRenderer, askRenderer)
}
