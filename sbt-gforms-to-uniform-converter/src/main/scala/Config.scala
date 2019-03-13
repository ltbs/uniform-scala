package ltbs.uniform.gformsparser

protected[gformsparser] case class Config(
  addressClass: Option[String],
  journeyPackage: String,
  controllerPackage: Option[String],
  logicTableTests: Boolean
)
