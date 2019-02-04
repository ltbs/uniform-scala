package ltbs.uniform.sampleprograms

import org.atnos.eff._
import ltbs.uniform._
import cats.implicits._

object GreasySpoon {

  type Money = Int

  type GreasyStack = Fx2[UniformAsk[Int,?], UniformAsk[Boolean,?]]

  def greasySpoon[S : _uniformAsk[Int,?] : _uniformAsk[Boolean,?]] : Eff[S,Money] = for {
    age           <- uask[Int, S]("age")
    food          <- uask[Boolean, S]("wantFood")
    tea           <- uask[Boolean, S]("wantTea")
    baconCost     <- uask[Int, S]("bacon").map(_ * 12) emptyUnless food
    eggsCost      <- uask[Int, S]("eggs").map(_ * 24) emptyUnless food
    foodCost      = baconCost + eggsCost
    teaCost       <- uask[Int, S]("sugar").map(_ * 10 + 50) emptyUnless tea
    youngDiscount = if (age < 16) teaCost / 10 else 0
    oldDiscount   = if (age > 60) (teaCost + foodCost) * (Math.min(age - 60,25) / 100) else 0
  } yield (foodCost + teaCost + youngDiscount + oldDiscount)

}
