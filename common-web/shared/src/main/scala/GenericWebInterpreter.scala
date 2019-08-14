package ltbs.uniform
package common.web

import shapeless._

trait GenericWebInterpreter[Html] {

  type WebTell[A] = GenericWebTell[A, Html]
  type WMC[A] = WebMonadConstructor[A, Html]
  type WM[A] = WebMonad[A, Html]

  def create[
    SupportedTell <: HList,
    SupportedAsk  <: HList
  ](messages: UniformMessages[Html])(
    implicit tellSummoner : TypeclassList[SupportedTell, WebTell],
    webMonadSummoner      : TypeclassList[SupportedAsk, WMC]
  ) = new Language[WebMonad[?,Html], SupportedTell, SupportedAsk]{
    def interact[Tell, Ask](
      id: String,
      tell: Tell,
      defaultIn: Option[Ask],
      validationIn: List[List[Rule[Ask]]],
      customContent: Map[String,(String, List[Any])]
    )(implicit
      selectorTell: IndexOf[SupportedTell,Tell],
      selectorAsk: IndexOf[SupportedAsk,Ask]
    ): WebMonad[Ask,Html] = {
      val customMessages = messages withCustomContent customContent
      val tellHtml = tellSummoner.forType[Tell].render(tell, id, customMessages)
      webMonadSummoner.forType[Ask].apply(
        id,
        tellHtml,
        defaultIn,
        validationIn,
        customMessages
      )
    }
  }

}
