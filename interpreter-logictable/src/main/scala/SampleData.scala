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
