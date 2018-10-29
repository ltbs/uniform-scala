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

import org.atnos.eff._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import UniformTest._
import cats.data.Writer

object LogicTableInterpreter {

  type LogicTableStack = Fx.fx3[Either[String,?], Writer[String,?], List]

  trait Examples[T] {
    def examples(key: String): List[T]
  }

  implicit def listToExamples[A](in: List[A]): Examples[A] = new Examples[A] {
    def examples(key: String): List[A] = in
  }

  implicit class UniformListEffectOps[R, A](e: Eff[R, A]) {
    type _either[Q] = Either[String,?] |= Q
    type _writer[Q] = Writer[String,?] |= Q

    def giveExamples[C, U](
      reader: Examples[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      eitherM: _either[U],
      writerM:_writer[U],
      listM:_list[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformAsk[C,?], U] {
        def apply[X](ax: UniformAsk[C,X]): Eff[U, X] =
          ax match {
            case UniformAsk(key,v) =>
              val i: Eff[U,X] = for {
                a <- ListEffect.values(reader.examples(key):_*)
                vu = a.asInstanceOf[X]
                _ <- WriterEffect.tell(s"$key:$vu")
                va <- send(v(vu).toEither)
              } yield va
              i
          }
      })
  }

}
