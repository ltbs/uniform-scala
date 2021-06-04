package ltbs.uniform
package common.web

import validation.Rule

trait WebAskList[A, Html] {

  def deleteConfirmationJourney: Uniform[Needs[_], Any, Boolean]

  def apply(
    key: String,
    askJourney: (Option[Int], List[A]) => WebMonad[A,Html],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
  ): WebMonad[List[A],Html]

}
