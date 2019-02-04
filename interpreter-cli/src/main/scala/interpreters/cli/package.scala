package ltbs.uniform.interpreters

import org.atnos.eff._
import cats._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import cats.implicits._
import scala.util.Try
import ltbs.uniform._
import scala.io.StdIn.readLine

package object cli { 

  implicit class UniformCliEffectOps[R, A](e: Eff[R, A]) {
    def using[C, U](
      f: String => C
    )(
      implicit member: Member.Aux[UniformAsk[C,?], R, U],
      evalM:_eval[U]
    ): Eff[U, A] =
      e.translate(
        new Translate[UniformAsk[C,?], U] {
          def apply[X](ax: UniformAsk[C,X]): Eff[U, X] =
            ax match {
              case UniformAsk(key,default,v) =>
                send(
                  Eval.later{
                    @annotation.tailrec 
                    def read(): X = {
                      print(s"$key: ")
                      val s = Try(f(readLine())).toEither.leftMap { _.getLocalizedMessage }
                      s.flatMap{x => v(x.asInstanceOf[C]).toEither} match {
                        case Left(err) =>
                          println("Error: " ++ err.toString)
                          read()
                        case Right(value) => value.asInstanceOf[X]
                      }
                    }

                    read
                  }
                )
            }
        })


    def usingList[C, U](
      f: String => C
    )(
      implicit member: Member.Aux[UniformAskList[C,?], R, U],
      evalM:_eval[U]
    ): Eff[U, A] =
      e.translate(
        new Translate[UniformAskList[C,?], U] {
          def apply[X](ax: UniformAskList[C,X]): Eff[U, X] =
            ax match {
              case UniformAskList(key,min,max,vElement,vList) =>
                send(
                  Eval.later{
                    @annotation.tailrec 
                    def read(): X = {
                      print(s"$key (comma separated): ")

                      val s = readLine().split(",").toList.map( x => 
                        Try(f(x)).toEither.leftMap { _.getLocalizedMessage }
                      ).sequence
                      s match {
                        case Left(err) =>
                          println("Error: " ++ err.toString)
                          read()
                        case Right(value) => value.asInstanceOf[X]
                      }
                    }

                    read
                  }
                )
            }
        })
  }
}
