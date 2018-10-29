/*
 * Copyright 2018 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
