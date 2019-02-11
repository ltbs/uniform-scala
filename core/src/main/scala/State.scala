package ltbs.uniform

import org.atnos.eff._, all._, syntax.all._
import cats.data.State

object DB {

  type _db[Q]  = State[(DB, List[String]),?] |= Q

  def readSerialised[S : _db[?]](id: String): Eff[S, Option[Encoded]] =
    get[S, (DB, List[String])].map { _._1.get(id) }

  def clear[S : _db[?]](key: String): Eff[S, Unit] = for {
    existing    <- get[S, (DB, List[String])]
    (dbSerialised, bread) = existing
    dbUpdated = dbSerialised - key
    _ = {println(s"Clearing $key - " ++ dbSerialised.keys.toString ++ " => " ++ dbUpdated.keys.toString)}
    _           <- put[S, (DB, List[String])]((dbUpdated, bread))
  } yield (())


}
