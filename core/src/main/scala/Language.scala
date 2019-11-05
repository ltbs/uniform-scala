package ltbs.uniform

import shapeless.HList
import scala.language.higherKinds
import com.github.ghik.silencer.silent

import validation.Rule

/** The core language of uniform, journeys will typically be expressed
  * in terms of this interaction 
  * 
  * {{{
  * import ltbs.uniform._
  * import scala.language.higherKinds
  * 
  * // the data types that the user can be presented with in a `tell'
  * type TellTypes = String :: Option[String] :: NilTypes
  * 
  * // the data types that the user can be prompted for in an `ask'
  * type AskTypes = Int :: Option[String] :: NilTypes
  * 
  * def program[F[_]: Monad](
  *   interpreter: Language[F, TellTypes, AskTypes]
  * ): F[(Int, Option[String])] = {
  *   import interpreter._
  *   for {
  *     a <- interact[String,Int]("ask-a", "please provide a")
  *     b <- interact[String,Int]("ask-b", "please provide b")
  *     c <- ask[Option[String]]("c")
  *     _ <- tell[Option[String]]("d", c)
  *   } yield ((a + b, c))
  * }
  * }}}
  */
trait Language[UF[_], SupportedTell <: HList, SupportedAsk <: HList]{

  /** Nest a journey within a single stage of a bigger journey. */
  def subJourney[A](@silent("never used") id: String)(sub: => UF[A]): UF[A] = sub

  /** Present an instance of Tell to the user, and ask for an
    * instance of Ask. Defines a 'step' in the user journey. 
    * 
    * {{{
    * val askAddr: UF[Address] = 
    *   interpreter.interact[Address,Boolean](
    *     "confirm-address", 
    *     someAddress
    *   )
    * }}}
    * 
    * @param id step identifier - care should be taken to keep these
    *           distinct within a given journey
    * @param tell data to be presented to the user
    * @param default optional existing/default value. For example an
    *           'edit' journey. 
    * @param validation rules to check the data is valid (after it has
    *           been turned into an Ask). Rules in the inner lists are
    *           error-accumulating, where as the outer groups are run
    *           sequentially.
    * @param customContent overrides any messages used in the journey
    *           step 
    */  
  def interact[Tell, Ask](
    id: String,
    tell: Tell,
    default: Option[Ask] = None,
    validation: List[Rule[Ask]] = Nil,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(
    implicit
    selectorTell : IndexOf[SupportedTell, Tell],
    selectorAsk : IndexOf[SupportedAsk, Ask]
  ): UF[Ask]

  /** 
    * Ask the user for an instance of A. This is equivalent
    * to `interact[Unit, A]`
    * 
    * {{{
    * val askAddr: UF[Address] = 
    *   interpreter.ask[Address]("delivery-address")
    * }}}
    * 
    * @param id step identifier - care should be taken to keep these
    *           distinct within a given journey
    * @param default optional existing/default value. For example an
    *           'edit' journey. 
    * @param validation rules to check the data is valid (after it has
    *           been turned into an Ask). Rules in the inner lists are
    *           error-accumulating, where as the outer groups are run
    *           sequentially.
    * @param customContent overrides any messages used in the journey
    *           step 
    * 
    */    
  def ask[A](
    id: String,
    default: Option[A] = None,
    validation: List[Rule[A]] = Nil,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(
    implicit selectorAsk : IndexOf[SupportedAsk, A],
    selectorTell : IndexOf[SupportedTell, Unit]
  ) = interact[Unit,A](id, (), default, validation, customContent)

  /**
    * Present the user with an instance of A. This is equivalent
    * to `interact[A, Unit]`
    * 
    * {{{
    * val askAddr: UF[Unit] = 
    *   interpreter.tell[OrderConfimation](
    *     "order-confirmation", 
    *     myOrder.getConfirmation
    *   )
    * }}}
    * 
    * @param id step identifier - care should be taken to keep these
    *           distinct within a given journey
    * @param tell data to be presented to the user
    * @param customContent overrides any messages used in the journey
    *           step 
    */    
  def tell[A](
    id: String,
    tell: A,
    customContent: Map[String,(String,List[Any])] = Map.empty
  )(
    implicit selectorAsk : IndexOf[SupportedAsk, Unit],
    selectorTell : IndexOf[SupportedTell, A]
  ) = interact[A,Unit](id, tell, customContent=customContent)

}
