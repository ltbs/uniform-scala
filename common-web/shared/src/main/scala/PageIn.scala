package ltbs.uniform
package common.web

case class PageIn[Html](
  targetId: List[String],
  breadcrumbs: Breadcrumbs,
  request: Option[Input],
  state: DB,
  pathPrefix: List[String],
  config: JourneyConfig,
  messages: UniformMessages[Html]
) {
  def toPageOut[A](
    output: AskResult[A, Html],
    stateManipulation: DB => DB = identity
  ): PageOut[A,Html] = PageOut[A, Html](
    breadcrumbs,
    stateManipulation(state),
    output,
    pathPrefix,
    config
  )
}
