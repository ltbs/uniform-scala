package ltbs.uniform.sampleprograms

import org.atnos.eff._
import ltbs.uniform._

object SingleAndList {

  type UFInt[A] = UniformAsk[Int,A]
  type UFIntString[A] = Uniform[Int,String,A]

  def program[R: _uniformAsk[Int,?] : _uniform[Int,String,?]]: Eff[R, String] =
    for {
      a <- uask[Int, R]("single")
      i <- uniform[Int, String, R]("i", a)
    } yield (i)

}
