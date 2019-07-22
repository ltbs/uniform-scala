package ltbs.uniform

import cats.implicits._
import language.higherKinds
import shapeless._, labelled._

abstract class StepPerField[UF[_]: cats.Applicative, SupportedTell <: HList, SupportedAsk <: HList] extends Language[UF, SupportedTell, SupportedAsk] {

  trait AskStep[A] {
    def asker(
      id: String,
      default: Option[A] = None,
      validation: List[List[Rule[A]]] = Nil,
      customContent: Map[String,(String,List[Any])] = Map.empty
    ): UF[A]
  }

  implicit val askHNil = new AskStep[HNil] {
    def asker(
      id: String,
      default: Option[HNil] = None,
      validation: List[List[Rule[HNil]]] = Nil,
      customContent: Map[String,(String,List[Any])] = Map.empty
    ): UF[HNil] = ???
  }

  implicit def askHCons[K <: Symbol, H, T <: HList](
    implicit witness: Witness.Aux[K],
    hParser: Lazy[AskStep[H]],
    tParser: AskStep[T]
  ) = new AskStep[FieldType[K, H] :: T] {
    def asker(
      id: String,
      default: Option[FieldType[K, H] :: T] = None,
      validation: List[List[Rule[FieldType[K, H] :: T]]] = Nil,
      customContent: Map[String,(String,List[Any])] = Map.empty
    ): UF[FieldType[K, H] :: T] = (hParser.value.asker(witness.value.name),tParser.asker("")).mapN{field[K](_) :: _}
  }

  implicit def askerGeneric[A, T](
    id: String,
    default: Option[A] = None,
    validation: List[List[Rule[A]]] = Nil,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(implicit
    generic: LabelledGeneric.Aux[A,T],
    hGenParser: Lazy[AskStep[T]],
    lp: LowPriority
  ): UF[A] = hGenParser.value.asker(id, default.map{generic.to}, Nil, customContent).map{generic.from}

  def askStepPerField[A](
    id: String,
    default: Option[A] = None,
    validation: List[List[Rule[A]]] = Nil,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(
    implicit asker: AskStep[A]
  ): UF[A] = asker.asker(id, default, validation, customContent)

}
