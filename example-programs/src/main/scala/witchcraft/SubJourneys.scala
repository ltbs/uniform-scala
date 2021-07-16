package ltbs.uniform
package examples.witchcraft

package object subjourneys {

  def subjourneyProg = for {
    _ <- ask[String]("begin", default = Some("x"))
    _ <- subJourney("one") { for {
      _ <- ask[String]("a", default = Some("x"))
      _ <- ask[String]("b", default = Some("x"))
      _ <- subJourney("half") { for {
        _ <- ask[String]("a", default = Some("x"))
        _ <- ask[String]("b", default = Some("x"))
      } yield (()) }
    } yield (()) }
    _ <- subJourney("twoa","twob") { for {
      _ <- ask[String]("a", default = Some("x"))
      _ <- ask[String]("b", default = Some("x"))
    } yield (()) }
    _ <- subJourney("threea","threeb", "threec") { ask[String]("v") }
    _ <- subJourney("threez","threeb", "threec") { ask[String]("a") }    
    _ <- subJourney("solo") { ask[String]("a") }
    _ <- ask[String]("end")
  } yield ()

}
