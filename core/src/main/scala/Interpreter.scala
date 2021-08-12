package ltbs.uniform

import scala.language.higherKinds
import scala.language.experimental.macros
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag
import cats.~>;

/** Maps a journey from its abstract form into a concrete one. 
  * 
  * Must target a specific higher kinded type (for example, WebMonad). 
  * 
  * When creating your own interpreter you may be better served by
  * inheriting MonadInterpreter as this already has some of the wiring 
  * 
  */
trait Interpreter[F[_], INTERACTTC[_,_], ASKLISTTC[_]] {

  protected def convertImpl[E[_], A](key: String, in: () => E[A], transformation: Converter[E, F, A]): F[A] = 
    transformation(key, in)

  def interpretImpl[H <: Needs[_,_], T: Tag, A: Tag, E[_]](
    program: Uniform[H, T, A], 
    interactMap: Map[(LightTypeTag, LightTypeTag), INTERACTTC[_,_]],    
    convertMap: Map[(LightTypeTag, LightTypeTag), Any], // effectively Map[(TagE, TagA), Converter[E,A,_]]. I hope to remove this field and have it handled directly in the macros
    listAskMap: Map[LightTypeTag, ASKLISTTC[_]]    
  ): F[A]

  def transform[G[_]](f: F ~> G) = {
    val that = this
    new Interpreter[G, INTERACTTC, ASKLISTTC] {
      def interpretImpl[H <: Needs[_,_], T: Tag, A: Tag, E[_]](
        program: Uniform[H, T, A],
        interactMap: Map[(LightTypeTag, LightTypeTag), INTERACTTC[_,_]],
        convertMap: Map[(LightTypeTag, LightTypeTag), Any],
        listAskMap: Map[LightTypeTag, ASKLISTTC[_]]
      ): G[A] = f(that.interpretImpl[H, T, A, E](program, interactMap, convertMap, listAskMap))
    }
  }

  /** Convert the supplied abstract journey into the target type
    *
    * @param program - the uniform journey to be interpreted 
    * @return the journey in it's new interpreted representation
    */
  def interpret[H <: Needs[_,_],T, A](
    program: Uniform[H, T, A]
  ): F[A] = macro InterpreterMacros.interpreter_impl[H, A, INTERACTTC, ASKLISTTC, F, T]
}
