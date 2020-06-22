package ltbs.uniform

import scala.language.higherKinds

import cats.implicits._
import ltbs.uniform.{Uniform => U}
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.Tag
import validation.Rule

trait MonadInterpreter[F[+_], ASKTC[_], TELLTC[_]] extends Interpreter[F, ASKTC, TELLTC]{

  implicit def monadInstance: cats.Monad[F]

  protected def askImpl[A](key: String, default: Option[A], validation: Rule[A], asker: ASKTC[A]): F[A]
  protected def tellImpl[T](key: String, value: T, teller: TELLTC[T]): F[Unit]
  protected def interactImpl[A,T](key: String, tellValue: T, default: Option[A], validation: Rule[A], asker: ASKTC[A], teller: TELLTC[T]): F[A] =
    tellImpl(key, tellValue, teller) >> askImpl(key, default, validation, asker)    
  protected def endTellImpl[T](key: String, value: T, teller: TELLTC[T]): F[Nothing] =
    tellImpl(key, value, teller) >> endImpl(key)
  protected def endImpl(key: String): F[Nothing]
  protected def subjourneyImpl[A](path: List[String], inner: F[A]): F[A] = inner

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
        tellImpl[T](key, value, tellMap(tag.tag).asInstanceOf[TELLTC[T]])
      case U.Interact(key, value, default, validation, askTag, tellTag: Tag[T]) =>
        interactImpl[A, T](key, value, default, validation, askMap(askTag.tag).asInstanceOf[ASKTC[A]], tellMap(tellTag.tag).asInstanceOf[TELLTC[T]])
      case U.Ask(key, default, validation, tag) =>
        askImpl(key, default, validation, askMap(tag.tag).asInstanceOf[ASKTC[A]])
      case U.EndTell(key, value, tag: Tag[T]) =>
        endTellImpl[T](key, value, tellMap(tag.tag).asInstanceOf[TELLTC[T]])
      case U.End(key) =>
        endImpl(key)
      case U.Pure(v) =>
        v.pure[F]
      case U.Subjourney(path, inner) =>
        subjourneyImpl(path, executeImpl(inner, askMap, tellMap))
    }
  }


}
