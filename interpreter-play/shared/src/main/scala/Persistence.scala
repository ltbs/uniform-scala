package ltbs.uniform
package interpreters.playframework

import play.api._,mvc._
import concurrent.{Future,ExecutionContext}
import java.util.UUID

trait PersistenceEngine[A <: Request[AnyContent]] {
  def apply(request: A)(f: DB => Future[(DB,Result)]): Future[Result]
}

case class DebugPersistence(underlying: UUIDPersistence)(implicit ec: ExecutionContext) extends UUIDPersistence {

  val log: Logger = Logger("persistence")  

  def load(uuid: UUID): Future[DB] = 
    underlying.load(uuid).map{ r => 
      log.info(s"load($uuid): ${r.toString}")
      r
    }

  def save(uuid: UUID, db: DB): Future[Unit] = {
    underlying.save(uuid, db).map{ case _ => 
      log.info(s"save($uuid, ${db.toString})")
    }
  }
}

abstract class UUIDPersistence()(implicit ec: ExecutionContext) extends PersistenceEngine[Request[AnyContent]] {
  def load(uuid: UUID): Future[DB]
  def save(uuid: UUID, db: DB): Future[Unit]
  def apply(request: Request[AnyContent])(f: DB ⇒ Future[(DB,Result)]): Future[Result] = {

    val uuid: UUID = request.session.get("uuid").map{UUID.fromString}
      .getOrElse( UUID.randomUUID )

    for {
      db              ← load(uuid)
      (newDb, result) ← f(db)
      _               ← save(uuid, newDb)
    } yield result.withSession(
      request.session + ("uuid" → uuid.toString)
    )
  }
}

final case class UnsafePersistence(
  var state: Map[UUID,DB] = Map.empty
)(implicit ec: ExecutionContext) extends UUIDPersistence()(ec) {

  def load(uuid: UUID): Future[DB] =
    Future.successful(state.withDefaultValue(Map.empty)(uuid))

  def save(uuid: UUID,db: DB): Future[Unit] = {
    state = state + (uuid -> db)
    Future.successful(())
  }
}
