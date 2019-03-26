package ltbs.uniform

import cats.data.Validated
import cats.implicits._
import org.atnos.eff.Eff

case class UniformB[IN, OUT] private (
  key: String,
  tell: IN,
  default: Option[OUT],
  validation: OUT => Validated[String,OUT],
  customContent: Map[String,(String,List[Any])]
) {
  def validating(error: String, newValidation: Function[OUT,Boolean]): UniformB[IN, OUT] =
    UniformB(key,tell,default, { v =>
      (validation(v) andThen {x => Validated.cond(newValidation(x),x,error)}) }, customContent)

  def defaultOpt(out: Option[OUT]): UniformB[IN, OUT] =
    UniformB(key,tell,out, validation, customContent)

  def defaultTo(out: OUT): UniformB[IN, OUT] =
    UniformB(key,tell,Some(out), validation, customContent)

  def emptyUnlessPred[R :_uniform[IN, OUT, ?]  : _uniformCore](b: => Boolean)(implicit monoid: cats.Monoid[OUT]): Eff[R, OUT] = {
    if(b) (uniformBToStack(this)) else Eff.pure[R,OUT](monoid.empty)
  }

  def emptyUnless[R :_uniform[IN, OUT, ?]  : _uniformCore](eb: Eff[R,Boolean])(implicit monoid: cats.Monoid[OUT]): Eff[R,OUT] = for {
    opt <- eb
    ret <- if (opt) (uniformBToStack(this)) else Eff.pure[R,OUT](monoid.empty)
  } yield ret

  def withCustomContentAndArgs(newCustom: (String,(String, List[Any]))*): UniformB[IN, OUT] = {
    val combinedCustom = {Map(newCustom:_*).mapValues(List(_)) |+| customContent.mapValues(List(_))}.mapValues(_.head)
    UniformB(key,tell,default, validation, combinedCustom)
  }

  def withCustomContent(newCustom: (String,String)*): UniformB[IN, OUT] = {
    def f(in: (String,String)): (String,(String,List[Any])) = (in._1, (in._2, Nil))
    withCustomContentAndArgs(newCustom.map(f):_*)
  }

  def in[R :_uniform[IN, OUT, ?]  : _uniformCore]: Eff[R,OUT] = uniformBToStack(this)
} 

case class Uniform[IN, OUT, STACK] private (
  key: List[String],
  tell: IN,
  default: Option[OUT],
  validation: OUT => Validated[String,OUT],
  customContent: Map[String,(String,List[Any])]  
) {
  def validating(error: String, newValidation: Function[OUT,Boolean]): Uniform[IN, OUT, STACK] =
    Uniform(key,tell,default, { v =>
      (validation(v) andThen {x => Validated.cond(newValidation(x),x,error)}) }, customContent)

  def defaultingTo(out: OUT): Uniform[IN, OUT, STACK] =
    Uniform(key,tell,Some(out), validation, customContent)
  
}

object Uniform {
  def ask[OUT,STACK](keyH: String, keyT: String*) =
    Uniform[Unit,OUT,STACK](keyH :: keyT.toList, (), None, {v:OUT => v.valid}, Map.empty)
}

sealed trait UniformSelect[L,V] {
  def key: List[String]
  def validation: V => Validated[String,V]
}

object UniformAsk {
  def unapply[STACK, IN, OUT](
    in: Uniform[IN, OUT, STACK]
  ): Option[(List[String], Option[OUT], OUT => Validated[String,OUT])] =
    in match {
      case Uniform(key, _, default, validation, _) => Some((key,default, validation))
    }
}

object UniformTell {
  def unapply[STACK, IN, OUT](
    in: Uniform[IN, OUT, STACK]
  ): Option[(List[String], IN)] =
    in match {
      case Uniform(key, tell, _, _, _) => Some((key,tell))
    }
}


case class UniformAskList[L, V](
  key: List[String],
  min: Int = 0,
  max: Int = Int.MaxValue,
  validationElement: V => Validated[String,V] = {v:V => v.valid},
  validationList: List[V] => Validated[String,List[V]] = {v:List[V] => v.valid}    
)

case class UniformSubjourney[L, V](
  key: String,
  components: Eff[L,V]
)

case class UniformSelectOne[L, V](
  key: List[String],
  options: Set[V],
  validation: V => Validated[String,V] = {v:V => v.valid}
) extends UniformSelect[L,V]

case class UniformSelectMany[L, V](
  key: List[String],
  options: Set[V],
  min: Int = 0,
  max: Int = Int.MaxValue,
  validation: Set[V] => Validated[String,Set[V]] = {v:Set[V] => v.valid}
) extends UniformSelect[L,Set[V]] {
  require(min <= max, s"Cannot have between $min and $max items")
  require(min < options.size, s"Must choose more items than are available")
}
