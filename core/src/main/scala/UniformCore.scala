package ltbs.uniform

case class UniformCore(
  state: Map[List[String],Encoded] = Map.empty,
  breadcrumbs: List[List[String]] = Nil,
  path: List[String] = Nil
)
