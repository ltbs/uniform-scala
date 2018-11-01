package ltbs.uniform.interpreters

import scala.language.implicitConversions

import ltbs.uniform._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.data.Writer

package object logictable {

  type LogicTableStack = Fx.fx3[Either[String,?], Writer[String,?], List]

  type Examples[T] = Function[String, List[T]]

  implicit def listToExamples[A](in: List[A]): Examples[A] = _ => in
  implicit def partialToExamples[A](in: PartialFunction[String,List[A]]): Examples[A] = x => in(x)

  implicit class UniformListEffectOps[R, A](e: Eff[R, A]) {
    type _either[Q] = Either[String,?] |= Q
    type _writer[Q] = Writer[String,?] |= Q

    def giveExamples[C, U](
      reader: Examples[C]
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      eitherM: _either[U],
      writerM:_writer[U],
      listM:_list[U]
    ): Eff[U, A] = e.translate(
      new Translate[UniformAsk[C,?], U] {
        def apply[X](ax: UniformAsk[C,X]): Eff[U, X] =
          ax match {
            case UniformAsk(key,v) =>
              val i: Eff[U,X] = for {
                a <- ListEffect.values(reader(key):_*)
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
