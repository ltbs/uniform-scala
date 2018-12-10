package ltbs.uniform.prototype

import ltbs.uniform.datapipeline.Messages

case class CmsMessages(
  underlying: Map[String, List[String]]
) extends Messages {

  @annotation.tailrec
  private def replaceArgs(
    input: String,
    args: List[String],
    count: Int = 0
  ): String = args match {
    case Nil    => input
    case h :: t => replaceArgs(input.replace(s"[{]$count[}]", h), t, count+1)
  }

  @annotation.tailrec
  final def apply(key: List[String], args: Any*): String = key match {
    case Nil => ""
    case x::xs =>
      get(x, args:_*) match {
        case Some(o) => o
        case None => apply(xs, args:_*)
      }
  }

  def apply(key: String, args: Any*): String =
    get(key, args:_*).getOrElse(key)
 
  def get(key: String, args: Any*): Option[String] =
    list(key,args:_*).headOption

  def list(key: String, args: Any*): List[String] =
    underlying.getOrElse(key,Nil).map{
      replaceArgs(_,args.toList.map(_.toString))
    }
}
