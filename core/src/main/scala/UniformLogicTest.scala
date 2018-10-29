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
import cats.data.Writer
import UniformTest._
import LogicTableInterpreter._

object UniformLogicTest {

  val output = program[FxAppend[TestProgramStack, LogicTableStack]]
    .giveExamples(List(true,false))
    .giveExamples(
      for {
        lower <- (0L to 4L).toList.map(_ * 500000)
        higher <- (0L to 4L).toList.map(_ * 500000)
      } yield ((lower,higher)))
    .runEither
    .runWriter
    .runList
    .run

  def main(args: Array[String]): Unit = {
  output.foreach{ case (a,b) =>
    println(s"""${b.mkString(",")} => ${a.toValidated}""")
  }
  }

}
