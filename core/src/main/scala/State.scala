package ltbs.uniform

import org.atnos.eff._, all._, syntax.all._
import cats.data.State

object DB {

  type _db[Q]  = State[(DB, List[List[String]]),?] |= Q

  def readSerialised[S : _db[?]](id: List[String]): Eff[S, Option[Encoded]] =
    get[S, (DB, List[List[String]])].map { _._1.get(id) }

  def clear[S : _db[?]](key: List[String]): Eff[S, Unit] = for {
    existing    <- get[S, (DB, List[List[String]])]
    (dbSerialised, bread) = existing
    dbUpdated = dbSerialised - key
    _           <- put[S, (DB, List[List[String]])]((dbUpdated, bread))
  } yield (())

  def clearRecursive[S : _db[?]](key: List[String]): Eff[S, Unit] = for {
    existing    <- get[S, (DB, List[List[String]])]
    (dbSerialised, bread) = existing
    dbUpdated = dbSerialised.filterNot(_._1.startsWith(key))
    _           <- put[S, (DB, List[List[String]])]((dbUpdated, bread))
  } yield (())

}
