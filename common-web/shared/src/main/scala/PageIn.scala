package ltbs.uniform
package common.web

case class PageIn(
  targetId: List[String],
  path: Path,
  request: Option[Input],
  state: DB,
  pathPrefix: List[String]
)
