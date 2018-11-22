package ltbs.uniform.interpreters.playframework

import scala.concurrent.Future

trait Persistence {
  def dataGet: Future[DB]
  def dataPut(dataIn: DB): Future[Unit]
}

