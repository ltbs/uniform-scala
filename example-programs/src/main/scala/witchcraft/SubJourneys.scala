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
    _ <- i.ask[String]("begin")
    _ <- i.subJourney("one") { for {
      _ <- i.ask[String]("a")
      _ <- i.ask[String]("b")
    } yield (()) }
    _ <- i.subJourney("two","two") { for {
      _ <- i.ask[String]("a")
      _ <- i.ask[String]("b")
    } yield (()) }
    _ <- i.subJourney("three","three", "three") { i.ask[String]("a") }
    _ <- i.subJourney("solo") { i.ask[String]("a") }
    _ <- i.ask[String]("end")    
  } yield ()

}
