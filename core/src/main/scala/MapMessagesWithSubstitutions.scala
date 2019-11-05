package ltbs.uniform

private[uniform] case class MapMessagesWithSubstitutions(underlying: Map[String,List[String]]) extends UniformMessages[String] {

  @annotation.tailrec
  private def replaceArgs(
    input: String,
    args: List[String],
    count: Int = 0
  ): String = args match {
    case Nil    => input
    case h :: t => replaceArgs(input.replace(s"[{]$count[}]", h), t, count+1)
  }

  def get(key: String, args: Any*): Option[String] =
    underlying.get(key).flatMap{_.headOption}.map{
      replaceArgs(_,args.toList.map(_.toString))
    }

  override def get(key: List[String], args: Any*): Option[String] = {

    @annotation.tailrec
    def inner(innerkey: List[String]): Option[String] = {
      innerkey match {
        case Nil => None
        case x::xs =>
          get(x, args:_*) match {
            case Some(o) => Some(o)
            case None => inner(xs)
          }
      }
    }
    inner(key)
  }

  def list(key: String, args: Any*): List[String] =
    underlying.getOrElse(key,Nil).map{ x =>
      replaceArgs(x,args.toList.map(_.toString))
    }
}
