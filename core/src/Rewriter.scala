package ltbs.uniform

import scala.language.higherKinds

import shapeless._
import validation.Rule

/** Allows rewriting existing user journey without running them. 
  * 
  * This could be to optimise or otherwise transform a journey, for
  * example to swap one type of interaction with another (such as a
  * subjourney). 
  */
class Rewriter[TC[_], SupportedTell <: HList, SupportedAsk <: HList](
  val naive: Language[TC, SupportedTell, SupportedAsk]
) {

  trait RW[Ask] {

    def interact[Tell](
      id: String,
      tell: Tell,
      default: Option[Ask] = None,
      validation: Rule[Ask] = Rule.alwaysPass[Ask],
      customContent: Map[String,(String,List[Any])] = Map.empty
    ): TC[Ask]

  }

  type OptRW[A] = Option[RW[A]]

  def rewrite[RWI[_] <: RW[_]](
    implicit rewriteSummoner: TypeclassList[SupportedAsk, OptRW]
  ) = new Language[TC, SupportedTell, SupportedAsk] {
    def interact[Tell, Ask](
      id: String,
      tell: Tell,
      default: Option[Ask] = None,
      validation: Rule[Ask] = Rule.alwaysPass[Ask],
      customContent: Map[String,(String,List[Any])] = Map.empty
    )(
      implicit
        selectorTell : IndexOf[SupportedTell, Tell],
      selectorAsk : IndexOf[SupportedAsk, Ask]
    ): TC[Ask] = rewriteSummoner.forType[Ask] match {
      case Some(s) =>
        s.interact(id, tell, default, validation, customContent)
      case None =>
        naive.interact(id, tell, default, validation, customContent)
    }

  }
}
