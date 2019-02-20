package ltbs.uniform.sampleprograms

import org.atnos.eff._
import ltbs.uniform._

object SubJourneyExample {

  def program[R
      : _uniformCore
      : _uniformAsk[Int,?]
      : _uniform[Int,Unit,?]
  ]: Eff[R, Int] = for {
    one <- subjourney("one") { ask[Int]("a") } 
    two <- subjourney("two") {
             for {
               twoa <- ask[Int]("a")
               twob <- ask[Int]("b")
               _    <- ask[Int]("c") >>= {end("errorPage")(_)}
             } yield (twoa + twob)
    }
    three <- ask[Int]("three")
  } yield (one + two + three)

}
