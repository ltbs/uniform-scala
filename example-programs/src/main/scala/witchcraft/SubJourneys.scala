package ltbs.uniform
package examples.witchcraft

import cats.Monad
import cats.implicits._

package object subjourneys {

  type TellTypes = NilTypes
  type AskTypes = String :: NilTypes

  def subjourneyProg[F[_] : Monad](
    i: Language[F, TellTypes, AskTypes]
  ): F[Unit] = for {
    _ <- i.ask[String]("begin", default = Some("x"))
    _ <- i.subJourney("one") { for {
      _ <- i.ask[String]("a", default = Some("x"))
      _ <- i.ask[String]("b", default = Some("x"))
      _ <- i.subJourney("half") { for {
        _ <- i.ask[String]("a", default = Some("x"))
        _ <- i.ask[String]("b", default = Some("x"))
      } yield (()) }
    } yield (()) }
    _ <- i.subJourney("twoa","twob") { for {
      _ <- i.ask[String]("a", default = Some("x"))
      _ <- i.ask[String]("b", default = Some("x"))
    } yield (()) }
    _ <- i.subJourney("threea","threeb", "threec") { i.ask[String]("a") }
    _ <- i.subJourney("solo") { i.ask[String]("a") }
    _ <- i.ask[String]("end")    
  } yield ()

}
