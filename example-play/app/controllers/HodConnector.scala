package controllers

import ltbs.uniform.examples.beardtax._
import scala.concurrent._

class HodConnector(implicit ec: ExecutionContext) extends Hod[Future] {

  def recordBeardHeight(height: Int): Future[Unit] = Future{
    println(s"HOD CALL: $height")
  }

  def costOfBeard(beardStyle: BeardStyle, length: BeardLength): Future[Int] =
    Future{
      Thread.sleep(2000)
      IdDummyHod.costOfBeard(beardStyle, length)
    }
}
