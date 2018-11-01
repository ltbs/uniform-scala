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

package ltbs

import org.atnos.eff._
import cats.data.Validated
import org.atnos.eff.all.{none => _, _}
import cats.implicits._
import cats.Monoid

package object uniform {

  type _uniform[V,R] = UniformAsk[V,?] |= R
  type _uniformSelect[V,R] = UniformSelect[V,?] |= R

  def uask[R, T](key: String, validation: T => Validated[String,T] = {v:T => v.valid})(implicit member: UniformAsk[T, ?] |= R): Eff[R, T] =
    send[UniformAsk[T, ?], R, T](UniformAsk(key, validation))

  def uaskOneOf[R, T](key: String, options: Set[T], validation: T => Validated[String,T] = {v:T => v.valid})(implicit member: UniformSelect[T, ?] |= R): Eff[R, T] =
    send[UniformSelect[T, ?], R, T](UniformSelectOne(key, options, validation))

  def uaskNOf[R, T](
    key: String,
    options: Set[T],
    min: Int = 0,
    max: Int = Int.MaxValue,
    validation: Set[T] => Validated[String,Set[T]] = {v:Set[T] => v.valid}
  )(implicit member: UniformSelect[T, ?] |= R): Eff[R, Set[T]] =
    send[UniformSelect[T, ?], R, Set[T]](
      UniformSelectMany(key, options, min, Math.min(max, options.size), validation)
    )

  implicit class RichMonoidOps[R, A](e: Eff[R, A])(implicit monoid: Monoid[A]) {
    
    def emptyUnless(b: => Boolean): Eff[R, A] =
      if(b) e else Eff.pure[R,A](monoid.empty)

    def emptyUnless(eb: Eff[R,Boolean]): Eff[R,A] = for {
      opt <- eb
      ret <- if (opt) e else Eff.pure[R,A](monoid.empty)
    } yield ret

  }

  implicit class RichOps[R, A](wm: Eff[R, A]) {
    def when(b: => Boolean): Eff[R,Option[A]] =
      if(b) wm.map{_.some} else Eff.pure[R,Option[A]](none[A])

    def when(wmb: Eff[R,Boolean]): Eff[R,Option[A]] = for {
      opt <- wmb
      ret <- if (opt) wm map {_.some} else Eff.pure[R,Option[A]](none[A])
    } yield ret
  }

  def when[R, A](b: => Boolean)(wm: Eff[R, A]): Eff[R,Option[A]] =
    if(b) wm.map{_.some} else Eff.pure[R,Option[A]](none[A])

}

package uniform {
	object UniformEffect {} 
}