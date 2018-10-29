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

import cats._, data._
import org.atnos.eff._

import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Example {
  type ReaderInt[A] = Reader[Int, A]
  type WriterString[A] = Writer[String, A]

  type Stack = Fx.fx3[WriterString, ReaderInt, Eval]

  // useful type aliases showing that the ReaderInt and the WriterString effects are "members" of R
  // note that R could have more effects
  type _readerInt[R]    = ReaderInt |= R
  type _writerString[R] = WriterString |= R

  def program[R :_readerInt :_writerString :_eval]: Eff[R, Int] = for {
    // get the configuration
    n <- ask[R, Int]

    // log the current configuration value
    _ <- tell("the required power is "+n)

    // compute the nth power of 2
    a <- delay(math.pow(2, n.toDouble).toInt)

    // log the result
    _ <- tell("the result is "+a)
  } yield a

  // run the action with all the interpreters
  // each interpreter running one effect
  program[Stack].runReader(6).runWriter.runEval.run
}
