package ltbs.uniform.webmonad

import scala.concurrent.Future

trait Persistence {
  def dataGet: Future[DB]
  def dataPut(dataIn: DB): Future[Unit]
}
