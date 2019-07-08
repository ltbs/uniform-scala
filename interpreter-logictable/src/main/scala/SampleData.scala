package ltbs.uniform
package interpreters.logictable

trait SampleData[A] {
  def apply(key: String): List[A]
}

trait TellRenderer[A] {
  def apply(key: String, value: A): List[String]
}
