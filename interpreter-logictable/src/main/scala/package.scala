package ltbs.uniform.interpreters

import scala.language.implicitConversions

import ltbs.uniform._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.data.{Writer,State}
import cats.implicits._ // needed for scala 2.11 either flatMap

package object logictable {

  type LogicTableStack = Fx.fx4[State[UniformCore, ?], Either[String,?], Writer[String,?], List]

  type Examples[T] = Function[String, List[T]]

  implicit def listToExamples[A](in: List[A]): Examples[A] = _ => in
  implicit def partialToExamples[A](in: PartialFunction[String,List[A]]): Examples[A] = x => in(x)

  implicit class UniformListEffectOps[R, A](e: Eff[R, A]) {
    type _either[Q] = Either[String,?] |= Q
    type _writer[Q] = Writer[String,?] |= Q

    def giveExamples[OUT, U](
      reader: Examples[OUT]      
    )(
      implicit member: Member.Aux[Uniform[Unit,OUT,?], R, U],
      eitherM: _either[U],
      writerM:_writer[U],
      listM:_list[U]
    ): Eff[U, A] =
      e.translate(
        new Translate[Uniform[Unit,OUT,?], U] {
          def apply[X](ax: Uniform[Unit,OUT,X]): Eff[U, X] =
            ax match {
              case Uniform(key,_,_,v) =>
                val i: Eff[U,X] = for {
                  a <- ListEffect.values(reader(key.mkString("/")):_*)
                  _ <- WriterEffect.tell(s"${key.mkString(".")}:$a")
                  va <- send(v(a).toEither.map{_.asInstanceOf[X]})
                } yield (va)
                i
            }
        })

    // def giveExamples[C, U](
    //   reader: Examples[C]
    // )(
    //   implicit member: Member.Aux[UniformAsk[C,?], R, U],
    //   eitherM: _either[U],
    //   writerM:_writer[U],
    //   listM:_list[U]
    // ): Eff[U, A] = e.translate(
    //   new Translate[UniformAsk[C,?], U] {
    //     def apply[X](ax: UniformAsk[C,X]): Eff[U, C] =
    //       ax match {
    //         case UniformAsk(key,default,v) =>
    //           val i: Eff[U,C] = for {
    //             a <- ListEffect.values(reader(key):_*)
    //             vu = a.asInstanceOf[C]
    //             _ <- WriterEffect.tell(s"$key:$vu")
    //             va <- send(v(vu).toEither)
    //           } yield va
    //           i
    //       }
    //   })

    def giveSelectionExamples[C, U](
      reader: Examples[Set[C]]
    )(
      implicit member: Member.Aux[UniformSelect[C,?], R, U],
      eitherM: _either[U],
      writerM:_writer[U],
      listM:_list[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformSelect[C,?], U] {
        def apply[X](ax: UniformSelect[C,X]): Eff[U, X] =
          ax match {
            case UniformSelectOne(key,opts,v) =>
              val selections = reader(key.mkString("/")).flatten

              val i: Eff[U,X] = for {
                a <- ListEffect.values(selections:_*)
                vu = a.asInstanceOf[X]
                _ <- WriterEffect.tell(s"$key:$vu")
                va <- send(v(vu).toEither)
              } yield va
              i

            case UniformSelectMany(key,opts,min,max,v) =>
              val i: Eff[U,X] = for {
                a <- ListEffect.values(reader(key.mkString("/")):_*)
                vu = a.asInstanceOf[X]
                _ <- WriterEffect.tell(s"$key:$vu")
                va <- send(v(vu).toEither)
              } yield va
              i
          }
      })


    // def runLogicTable[U](
    //   implicit member1: Member.Aux[Either[String,?], R1, U],
    //   member2: Member.Aux[Writer[String,?], R2, R1],
    //   member3: Member.Aux[List, R3, R2]
    // ): Eff[U, A] = e.runEither.runWriter.runList.run
  }

}
