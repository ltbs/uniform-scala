package ltbs.uniform

import scala.language.higherKinds

import cats.implicits._
import ltbs.uniform.{Uniform => U}
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag
import validation.Rule

abstract class MonadInterpreter[F[+_]: cats.Monad, ASKTC[+_], TELLTC[_]] extends Interpreter[F, ASKTC, TELLTC]{

  def ask[A](key: String, default: Option[A], validation: Rule[A], asker: ASKTC[A]): F[A]
  def tell[T](key: String, value: T, teller: TELLTC[T]): F[Unit]
  def interact[A,T](key: String, tellValue: T, default: Option[A], validation: Rule[A], asker: ASKTC[A], teller: TELLTC[T]): F[A] =
    tell(key, tellValue, teller) >> ask(key, default, validation, asker)    
  def endTell[T](key: String, value: T, teller: TELLTC[T]): F[Nothing] =
    tell(key, value, teller) >> end(key)
  def end(key: String): F[Nothing]
  def subjourney[A](path: List[String], inner: F[A]): F[A] = inner

  def executeImpl[H <: Needs[_], A: Tag, T: Tag](
    program: Uniform[H, A, T], 
    askMap: Map[LightTypeTag, ASKTC[_]],    
    tellMap: Map[LightTypeTag, TELLTC[_]],
  ): F[A] = {
    program match {
      case U.Map(base, f) =>
        executeImpl(base, askMap, tellMap).map(f)
      case U.FlatMap(base, f) =>
        executeImpl(base, askMap, tellMap).flatMap(f.map(executeImpl(_, askMap, tellMap)))
      case U.Tell(key, value, tag: Tag[T]) =>
        tell[T](key, value, tellMap(tag.tag).asInstanceOf[TELLTC[T]])
      case U.Interact(key, value, default, validation, askTag, tellTag: Tag[T]) =>
        interact[A, T](key, value, default, validation, askMap(askTag.tag), tellMap(tellTag.tag).asInstanceOf[TELLTC[T]])
      case U.Ask(key, default, validation, tag) =>
        ask(key, default, validation, askMap(tag.tag))
      case U.EndTell(key, value, tag: Tag[T]) =>
        endTell[T](key, value, tellMap(tag.tag).asInstanceOf[TELLTC[T]])
      case U.End(key) =>
        end(key)
      case U.Pure(v) =>
        v.pure[F]
      case U.Subjourney(path, inner) =>
        subjourney(path, executeImpl(inner, askMap, tellMap))
    }
  }


}
