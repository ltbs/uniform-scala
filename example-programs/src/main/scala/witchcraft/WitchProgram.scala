package ltbs.uniform
package examples

import validation._

package object witchcraft {

  def witchProgram = for {
    f <- ask[List[Familiar]]("familiars", validation = Rule.lengthBetween(1, 3))
    e <- askList[Evidence]("evidence",
      validation = Rule.forEachInList(Rule.cond({_ != Evidence.HasWartOnNose}, "dont-care-about-warts"))) { case (index: Option[Int], existing: List[Evidence]) => 
        ask[Evidence]("ev1", default = index.map(existing))
    }
    a <- ask[Accused]("accused")
  } yield WitchReport(a,e,f)

}
