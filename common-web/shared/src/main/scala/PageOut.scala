package ltbs.uniform
package common.web

/** The combined output of web execution */
final case class PageOut[A,Html](
  path: Path,
  db: DB,
  output: AskResult[A,Html]
)
