package playexample

import org.scalatest._

import ltbs.uniform.sampleprograms.BeardTax._
import ltbs.uniform.interpreters.selenium._
import play.api._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import cats.implicits._
import org.openqa.selenium.{WebDriver, firefox}, firefox.FirefoxDriver

class TestSeleniumPlay extends FlatSpec with Matchers with BeforeAndAfterAll {

  val server = {
    val process = new play.core.server.RealServerProcess(Nil)
    play.core.server.ProdServerStart.start(process)
  }

  // def beforeAll(configMap: Map[String, Any]): Unit = {
  //   println(server.mainAddress) // force start
  // }

  def afterAll(configMap: Map[String, Any]): Unit = {
    server.stop
  }

  lazy val driver: WebDriver = new FirefoxDriver

  "The Play BeardTax Implementation" should "be navigable with selenium" in {

    {
      Thread.sleep(10000)
      driver.get("http://localhost:9000/")
    }

    implicit def beardStyleInputer: SeleniumInputer[BeardStyle] = ???
    implicit def beardLengthInputer: SeleniumInputer[BeardLength] = ???
    implicit def mopInputer: SeleniumInputer[Option[MemberOfPublic]] = ???

    program[FxAppend[TestProgramStack, SeleniumStack]]
      .giveExample(MemberOfPublic("luke", "tebbs", java.time.LocalDate.of(2000,1,1)).some)
      .giveExample(BeardStyle.Goatee : BeardStyle)
      .giveExample((1,1))
      .runReader(driver)
      .runEval

  }
  
}
