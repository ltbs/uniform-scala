package ltbs.uniform

import language.higherKinds
import shapeless._

class Rewriter[TC[_], SupportedTell <: HList, SupportedAsk <: HList](
  val naive: Language[TC, SupportedTell, SupportedAsk]
) {

  trait RW[Ask] {

    def interact[Tell](
      id: String,
      tell: Tell,
      default: Option[Ask] = None,
      validation: List[List[Rule[Ask]]] = Nil,
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
      validation: List[List[Rule[Ask]]] = Nil,
      customContent: Map[String,(String,List[Any])] = Map.empty
    )(
      implicit
        selectorTell : IndexOf[SupportedTell, Tell],
      selectorAsk : IndexOf[SupportedAsk, Ask]
    ): TC[Ask] = rewriteSummoner.forType[Ask] match {
      case Some(s) ⇒
        s.interact(id, tell, default, validation, customContent)
      case None ⇒
        naive.interact(id, tell, default, validation, customContent)
    }

  }
}
