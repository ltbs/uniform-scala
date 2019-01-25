package ltbs.uniform

import cats.data.Validated
import cats.implicits._
import org.atnos.eff.Eff

sealed trait UniformSelect[L,V] {
  def key: String
  def validation: V => Validated[String,V]
}

case class UniformAsk[L, V](
  key: String,
  validation: V => Validated[String,V] = {v:V => v.valid}
)

case class UniformAskList[L, V](
  key: String,
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
  key: String,
  options: Set[V],
  validation: V => Validated[String,V] = {v:V => v.valid}
) extends UniformSelect[L,V]

case class UniformSelectMany[L, V](
  key: String,
  options: Set[V],
  min: Int = 0,
  max: Int = Int.MaxValue,
  validation: Set[V] => Validated[String,Set[V]] = {v:Set[V] => v.valid}
) extends UniformSelect[L,Set[V]] {
  require(min <= max, s"Cannot have between $min and $max items")
  require(min < options.size, s"Must choose more items than are available")
}
