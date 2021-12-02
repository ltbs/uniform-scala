package ltbs.uniform
package examples

import validation._

package object witchcraft {

  def witchProgram = for {
    e <- askList[Evidence]("evidence",
      validation = Rule.nonEmpty[List[Evidence]] alongWith Rule.forEachInList(Rule.cond({_ != Evidence.HasWartOnNose}, "dont-care-about-warts"))
    ) {
      case (index: Option[Int], existing: List[Evidence]) =>
        ask[Evidence]("ev1", default = index.map(existing))
    }
    f <- askList[Familiar]("familiars",
      validation = Rule.minLength(1)
    ) {
      case (index: Option[Int], existing: List[Familiar]) =>
        ask[Familiar]("fam", default = index.map(existing))
    }
    a <- ask[Accused]("accused")
  } yield WitchReport(a,e,f)

}
