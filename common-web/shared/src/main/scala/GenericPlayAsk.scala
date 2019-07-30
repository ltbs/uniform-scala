package ltbs.uniform
package common.web

import concurrent.Future

/** The broadest representation of an `ask` interaction in the context
  * of web interpreters. Extend this if you want the most control over
  * the interaction, otherwise if you want a simple form consider
  * providing [[FormField]] instances instead.
  */
trait GenericWebAsk[A,Html] {

  def page(
    targetId: List[String],
    currentId: List[String],
    default: Option[A],
    validation: List[List[Rule[A]]],
    config: JourneyConfig,
    submittedData: Option[Input],
    path: Path,
    db: DB,
    messages: UniformMessages[Html]
  ): Future[PageOut[A,Html]]

}
