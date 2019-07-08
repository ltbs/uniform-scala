package ltbs.uniform.examples.beardtax

import scala.language.higherKinds
import cats.Id

trait Hod[F[_]] {
  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): F[Int]  
}

object IdDummyHod extends Hod[Id] {

  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Id[Int] = 
      beardStyle match {
      case BeardStyle.SoulPatch => length._2 / 10
      case _                    => length._1 + (length._2 - length._1) / 2
    }

}
