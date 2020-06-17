package ltbs.uniform
package common.web

/** The combined output of web execution */
final case class PageOut[+A,Html](
  breadcrumbs: Breadcrumbs,
  db: DB,
  output: AskResult[A,Html],
  pathPrefix: List[String],
  config: JourneyConfig
)
