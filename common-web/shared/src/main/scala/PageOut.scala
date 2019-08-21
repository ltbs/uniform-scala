package ltbs.uniform
package common.web

final case class PageOut[A,Html](
  path: Path,
  db: DB,
  output: AskResult[A,Html],
  pathPrefix: List[String]
)
