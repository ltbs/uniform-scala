package ltbs.uniform.datapipeline

trait Messages {
  def apply(key: String, args: Any*): String
  def get(key: String, args: Any*): Option[String]
  def list(key: String, args: Any*): List[String]
}

object NoopMessages extends Messages {
  def apply(key: String, args: Any*): String = key
  def get(key: String, args: Any*): Option[String] = None
  def list(key: String, args: Any*): List[String] = Nil
}
