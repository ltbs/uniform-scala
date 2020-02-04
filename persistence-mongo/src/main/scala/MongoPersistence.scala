package ltbs.uniform
package interpreters.playframework
package mongo

import play.api._, mvc._
import scala.concurrent.{ ExecutionContext, Future }

import reactivemongo.api.{ Cursor, DefaultDB, MongoConnection, AsyncDriver }
import reactivemongo.api.bson._
import concurrent.{Future,ExecutionContext}
import java.util.UUID
import scala.util.Try

case class Wrapper(
  id: String,
  data: DB
)

case class MongoPersistence[A <: Request[AnyContent]] (
  mongoUri: String = "mongodb://localhost:27017/mydb?authMode=scram-sha1",
  databaseName: String,
  collectionName: String
)(getId: A => String)(implicit ec: ExecutionContext) extends PersistenceEngine[A] {

  val driver = AsyncDriver()
  val parsedUri = MongoConnection fromString mongoUri
  val futureConnection = parsedUri.flatMap(driver.connect(_))

  def db1: Future[DefaultDB] = futureConnection.flatMap(_.database(databaseName))
  def collection = db1.map(_.collection(collectionName))

  implicit def dbWriter = new BSONDocumentWriter[DB] {
    def writeTry(t: DB): Try[BSONDocument] = {
      val elems: Iterable[(String, BSONValue)] = t.map {
        case (k,v) => ("/" + k.mkString("/"), BSONString(v))
      }
      Try(BSONDocument(elems))
    }
  }

  implicit def dbReader = new BSONDocumentReader[DB] {
    def readDocument(doc: BSONDocument): Try[DB] = Try(doc.toMap.map {
      case (k,BSONString(v)) => (k.split("/").toList,v)
    })
  }

  implicit def wrapWriter = Macros.writer[Wrapper]
  implicit def wrapReader = Macros.reader[Wrapper]  

  def apply(request: A)(f: DB => Future[(DB, Result)]): Future[Result] = {
    val selector = document("id" -> getId(request))

    collection.flatMap(_.find(selector).one[Wrapper]).flatMap {
      case Some(Wrapper(_, data)) => f(data)
      case None => f(DB.empty)
    } flatMap { case (newDb, result) => 
        collection.flatMap(_.update.one(selector, newDb).map(_ => result))
    }
  }

}
