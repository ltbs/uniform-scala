package ltbs.uniform.gformsparser

sealed trait Field {
  def id: String
  def label: String
  def mandatory: Boolean
  def validIf: Option[String]
  def includeIf: Option[String]
  def updateId(f: String => String): Field
  def helpText: Option[String]

  /** will only be defined for a non-generated Id */
  def saneId: Option[String] = Some(id).filterNot(_.startsWith("generatedId_"))
}

case class TextField(
  id: String,
  label: String,
  errorMessage: Option[String],
  helpText: Option[String],
  format: Option[String],
  mandatory: Boolean = true,
  validIf: Option[String],
  includeIf: Option[String]
) extends Field {
  def updateId(f: String => String) = this.copy(id = f(id))
}

case class InfoField(
  id: String,
  infoText: String,
  label: String,
  mandatory: Boolean = true,
  validIf: Option[String],
  includeIf: Option[String]
) extends Field{
  def updateId(f: String => String) = this.copy(id = f(id))
  def helpText: Option[String] = None
}

case class ChoiceField(
  id: String,
  label: String,
  choices: Set[String] = Set.empty,
  mandatory: Boolean = true,
  helpText: Option[String],  
  multivalue: Boolean = false,
  validIf: Option[String],
  includeIf: Option[String]
) extends Field{
  def updateId(f: String => String) = this.copy(id = f(id))

  def isDisguisedBoolean: Boolean = choices.size == 2
}

case class DateField(
  id: String,
  label: String,
  mandatory: Boolean = true,
  helpText: Option[String],  
  validIf: Option[String],
  includeIf: Option[String]
) extends Field{
  def updateId(f: String => String) = this.copy(id = f(id))
}

case class FileField(
  id: String,
  label: String,
  mandatory: Boolean = true,
  validIf: Option[String],
  includeIf: Option[String],
  helpText: Option[String]  
) extends Field {
  def updateId(f: String => String) = this.copy(id = f(id))
}

case class GroupField(
  id: String,
  label: String,
  fields: List[Field],
  mandatory: Boolean = true,
  validIf: Option[String],
  includeIf: Option[String],
  helpText: Option[String]  
) extends Field {
  def updateId(f: String => String) = this.copy(id = f(id))
}

case class AddressField(
  id: String,
  label: String,
  mandatory: Boolean = true,
  validIf: Option[String],
  includeIf: Option[String],
  helpText: Option[String]
) extends Field {
  def updateId(f: String => String) = this.copy(id = f(id))
}
