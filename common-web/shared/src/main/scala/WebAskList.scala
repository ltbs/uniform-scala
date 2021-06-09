package ltbs.uniform
package common.web

import validation.Rule

trait WebAskList[Html, A] {

  def deleteConfirmationJourney: Uniform[Needs[_], Any, Boolean]

  def apply(
    key: String,
    askJourney: (Option[Int], List[A]) => WebMonad[Html,A],
    default: Option[List[A]],
    validation: Rule[List[A]],
    customContent: Map[String,(String, List[Any])],
  ): WebMonad[Html, List[A]]

}
