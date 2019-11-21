package ltbs.uniform
package common.web

case class PageIn(
  targetId: List[String],
  breadcrumbs: Breadcrumbs,
  request: Option[Input],
  state: DB,
  pathPrefix: List[String],
  config: JourneyConfig
) {
  def toPageOut[A, Html](
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
