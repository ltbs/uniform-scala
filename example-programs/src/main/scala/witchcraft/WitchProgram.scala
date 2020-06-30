package ltbs.uniform
package examples

import validation._

package object witchcraft {

  def witchProgram = for {
    f <- askList[Familiar]("familiars", validation = Rule.lengthBetween(1, 3))
    e <- askList[Evidence]("evidence", validation =
      Rule.forEachInList(Rule.cond({_ != Evidence.HasWartOnNose}, "dont-care-about-warts")))
    a <- ask[Accused]("accused")
  } yield WitchReport(a,e,f)

}
