package ltbs.uniform.web

sealed trait ListControl

final case object Continue extends ListControl
final case object AddAnother extends ListControl
final case class Edit(ordinal: Int) extends ListControl
final case class Delete(ordinal: Int) extends ListControl
