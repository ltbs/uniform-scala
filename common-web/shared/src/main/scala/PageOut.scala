package ltbs.uniform
package common.web

final case class PageOut[A,Html](
  breadcrumbs: Breadcrumbs,
  db: DB,
  output: AskResult[A,Html],
  pathPrefix: List[String],
  config: JourneyConfig
)
