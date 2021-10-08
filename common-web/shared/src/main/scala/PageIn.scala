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

  def trackLeapPoint(state: DB): DB = {
    queryParams.get("leap-to") match {
      case Some(x :: Nil) =>
        println(state)
        val newState = state + (
          ("_leap-to"   :: Nil) -> x,
          ("_leap-from" :: Nil) -> targetId.mkString("/")
        )
        println(newState)
        newState
      case _       => state
    }
  }

  def toPageOut[A](
    output: AskResult[Html, A],
    stateManipulation: DB => DB = identity
  ): PageOut[Html,A] = PageOut[Html, A](
    breadcrumbs,
    trackLeapPoint(stateManipulation(state)),
    output,
    pathPrefix,
    config
  )
}
