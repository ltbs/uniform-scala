package ltbs.uniform
package common.web

/** The combined output of web execution */
final case class PageOut[Html, +A](
  breadcrumbs: Breadcrumbs,
  db: DB,
  output: AskResult[Html,A],
  pathPrefix: List[String],
  config: JourneyConfig
)
