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

case class SampleListQty[A](value: Int) extends AnyVal

case class LTInteraction[T, A](
  tellRenderer: TellRenderer[T],
  askRenderer: SampleData[A]
)

object LTInteraction {
  implicit def auto[T,A](
    implicit tellRenderer: TellRenderer[T],
    askRenderer: SampleData[A]
  ): LTInteraction[T,A] = LTInteraction(tellRenderer, askRenderer)

  val nothingRenderer: SampleData[Nothing] = new SampleData[Nothing] {
    override def apply(key: String): List[Nothing] = List.empty[Nothing]
  }

  implicit def fromTellNothing[T](
    implicit tellRenderer: TellRenderer[T],
  ): LTInteraction[T,Nothing] = LTInteraction[T, Nothing](tellRenderer, nothingRenderer)
  
}
