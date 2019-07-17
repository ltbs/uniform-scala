package ltbs.uniform

import shapeless.HList
import scala.language.higherKinds
import com.github.ghik.silencer.silent

trait Language[UF[_], SupportedTell <: HList, SupportedAsk <: HList]{

  def subJourney[A](@silent id: String)(sub: ⇒ UF[A]): UF[A] = sub

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
  ): UF[Ask]

  def ask[A](
    id: String,
    default: Option[A] = None,
    validation: List[List[Rule[A]]] = Nil,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(
    implicit selectorAsk : IndexOf[SupportedAsk, A],
    selectorTell : IndexOf[SupportedTell, Unit]
  ) = interact[Unit,A](id, (), default, validation, customContent)

  def tell[A](
    id: String,
    t: A,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(
    implicit selectorAsk : IndexOf[SupportedAsk, Unit],
    selectorTell : IndexOf[SupportedTell, A]
  ) = interact[A,Unit](id, t, customContent=customContent)

}
