package ltbs.uniform.sampleprograms

import org.atnos.eff._
import ltbs.uniform._

object SingleAndList {

  type UFInt[A] = UniformAsk[Int,A]
  type UFIntString[A] = Uniform[Int,String,A]

  def program[R
      : _uniformCore
      : _uniformAsk[Int,?]
      : _uniform[Int,String,?]
  ]: Eff[R, String] =
    for {
      a <- ask[Int]("single")
      i <- dialogue[Int, String]("i")(a)
    } yield (i)

}
