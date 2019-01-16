package ltbs.uniform.web

trait Messages {
  def apply(key: String, args: Any*): String
  def apply(key: List[String], args: Any*): String  
  def get(key: String, args: Any*): Option[String]
  def list(key: String, args: Any*): List[String]
}

object NoopMessages extends Messages {
  def apply(key: String, args: Any*): String = key
  def apply(key: List[String], args: Any*): String = key.head  
  def get(key: String, args: Any*): Option[String] = None
  def list(key: String, args: Any*): List[String] = Nil
}
