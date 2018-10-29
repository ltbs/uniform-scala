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

import cats.implicits._
import org.atnos.eff._
import cats.data.Validated
import UniformEffect._

object UniformTest {

  type _uniform[V,R] = UniformAsk[V,?] |= R

  type Litres = (Long,Long)

  implicit class RichVal[A](private val a:A) extends AnyVal {
    def check[B](pred: A => Boolean, error: B): Validated[B,A] =
      Validated.cond(pred(a), a, error)
    def checkEither[B](pred: A => Boolean, error: B): Either[B,A] =
      Either.cond(pred(a), a, error)
  }

  type TestProgramStack = Fx2[UniformAsk[Litres,?], UniformAsk[Boolean,?]]

  def program[R : _uniform[Litres,?] : _uniform[Boolean,?]]: Eff[R, String] = for {
    hiaz <- uask[R, Boolean]("hiaz")
    n <- uask[R, Litres]("litresProduced", validation = {case a@(l,h) => if (l > h) "lower cannot be more than higher".invalid else a.valid})
    s <- uask[R, Boolean]("imports")
    t <- uask[R, Boolean]("copacksForOthers")
    n <- uask[R, Litres]("copackedByOtherUk")
  } yield (s"$s AND $n")

}
