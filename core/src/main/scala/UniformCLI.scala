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
import cats._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.implicits._
import scala.util.Try
import UniformTest._

object UniformCliTest extends App {

  // implicit val stringReader: StringReader[String] = new StringReader[String] {
  //   def read(input: String): String = input
  // }

  // implicit val intReader: StringReader[Int] = new StringReader[Int] {
  //   def read(input: String): Int = input.toInt
  // }

  // implicit val longReader: StringReader[Long] = new StringReader[Long] {
  //   def read(input: String): Long = input.toLong
  // }

  implicit val booleanReader: StringReader[Boolean] = new StringReader[Boolean] {
    def read(input: String): Boolean = input.toLowerCase.startsWith("y")
  }

  implicit val litresReader: StringReader[Litres] = new StringReader[Litres] {
    def read(input: String): Litres = {
      val (l::h::_) = input.split(",").map{_.toLong}.toList
      (l,h)
    }
  }

  trait StringReader[T] {
    def read(input: String): T
  }

  implicit class UniformCliEffectOps[R, A](e: Eff[R, A]) {
    def runUniform[C, U](
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      evalM:_eval[U],
      reader: StringReader[C]
    ): Eff[U, A] =
      e.translate(
        new Translate[UniformAsk[C,?], U] {
          def apply[X](ax: UniformAsk[C,X]): Eff[U, X] =
            ax match {
              case UniformAsk(key,v) =>
                send(
                  Eval.later{
                    @annotation.tailrec 
                    def read(): X = {
                      print(s"$key: ")
                      val s = Try(reader.read(readLine())).toEither.leftMap { _.getLocalizedMessage }
                      s.flatMap{x => v(x.asInstanceOf[X]).toEither} match {
                        case Left(err) =>
                          println("Error: " ++ err)
                          read()
                        case Right(value) => value
                      }
                    }

                    read
                  }
                )
            }
        })

  }
  type Stack = Fx.fx3[UniformAsk[Litres,?], UniformAsk[Boolean,?], Eval]
  println("OUTPUT: " ++ program[Stack].runUniform.runUniform.runEval.run)
}

