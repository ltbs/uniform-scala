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
              case UniformAsk(key,v) =>
                send(
                  Eval.later{
                    @annotation.tailrec 
                    def read(): X = {
                      print(s"$key: ")
                      val s = Try(f(readLine())).toEither.leftMap { _.getLocalizedMessage }
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
}
