package ltbs.uniform.examples.beardtax

import scala.language.higherKinds

import cats.Id

trait Hod[F[_]] {
  def recordBeardHeight(height: Int): F[Unit]
  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): F[Int]  
}

object IdDummyHod extends Hod[Id] {

  def recordBeardHeight(height: Int): Id[Unit] = {
    println(height)
  }

  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Id[Int] = 
      beardStyle match {
      case BeardStyle.SoulPatch => length._2 / 10
      case _                    => length._1 + (length._2 - length._1) / 2
    }

}
