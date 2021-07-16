package ltbs.uniform

import scala.language.higherKinds
import cats.~>

/** Provides a mechanism to convert between two higher kinded types. 
  * 
  * This is commonly used to interleave other types into an
  * interpreted uniform journey (for example API calls returning a
  * Future[A] could be mapped to WebMonad[Html, A] in an
  * interpretation targetting WebMonad[Html, *]).  
  * 
  * If the journey contains convert(x: E[A]) then there must be an
  * implicit Converter[E, F, A] in scope when interpreting to the F 
  * higher kinded type. 
  * 
  * The most general way is to use a natural transformation E ~> F
  * (from all possible A convert E[A] to F[A]), which will be
  * implicitly converted into an Converter as needed
  * 
  * The intermediate form is to use a function from E[A] => F[A],
  * which is specific to the type A, and could differ from E[B] =>
  * F[B] in implementation. As with a natural transformation this will
  * be converted as needed. 
  * 
  * The least general form is to extend Converter, this makes it
  * possible to change the way the conversion is handled by key as 
  * well as by datatype. 
  * 
  * {{{
  * implicit val converter = new Converter[List, Option, Int] {
  *   def apply(key: String, in: () => List[Int]): Option[Int] = 
  *     key match {
  *       case "reverse" => in().lastOption
  *       case _ => in().headOption
  *     }
  * }
  * }}}
  * 
  */
@annotation.implicitNotFound("Could not find an implicit Converter[${E}, ${F}, ${A}], consider implementing an implicit ${E} ~> ${F} or ${E}[${A}] => ${F}[${A}] unless you need fine control.")
trait Converter[E[_], F[_], A] {
  def apply(key: String, in: () => E[A]): F[A]
}

object Converter {

  /** Create a very general converter from a natural transformation. 
    * 
    * {{{
    * implicit def listToOpt: List ~> Option = ???
    * def listToOpt1: Converter[List, Option, String] = implicitly
    * def listToOpt2: Converter[List, Option, Int] = implicitly
    * }}}
    * 
    *
    * @param natConv
    * @return Converter
    */
  implicit def natTransformToConverter[E[_], F[_], A](implicit natConv: E ~> F): Converter[E, F, A] =
    new Converter[E, F, A] {
      override def apply(key: String, in: () => E[A]): F[A] = natConv(in())
    }

  /** Create a converter from a function targetting a specific type. 
    * 
    * {{{
    * implicit def listToOpt: List[String] => Option[String] = ???
    * def good: Converter[List, Option, String] = implicitly
    * def bad: Converter[List, Option, Int] = implicitly // wont compile
    * }}}
    * 
    *
    * @param natConv
    * @return Converter
    */
  implicit def functionToConverter[E[_], F[_], A](implicit func: E[A] => F[A]): Converter[E, F, A] =
    new Converter[E, F, A] {
      override def apply(key: String, in: () => E[A]): F[A] = func(in())
    }
}
