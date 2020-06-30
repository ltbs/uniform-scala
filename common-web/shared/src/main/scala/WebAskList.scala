package ltbs.uniform
package common.web

import validation.Rule

trait WebAskList[A, Html] {

  def deleteConfirmationJourney: Uniform[Needs[_], Boolean, Any]

  def apply(
    key: String,
    askJourney: (Option[Int], List[A]) => WebMonad[A,Html],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
  ): WebMonad[List[A],Html]

}
