package ltbs.uniform
package common.web

case class PageIn[Html](
  targetId: List[String],
  breadcrumbs: Breadcrumbs,
  request: Option[Input],
  state: DB,
  pathPrefix: List[String],
  config: JourneyConfig,
  messages: UniformMessages[Html],
  queryParams: Map[String, Seq[String]]
) {

  def toPageOut[A](
    output: AskResult[Html, A],
    stateManipulation: DB => DB = identity
  ): PageOut[Html,A] = PageOut[Html, A](
    breadcrumbs,
    stateManipulation(state),
    output,
    pathPrefix,
    config
  )
}
