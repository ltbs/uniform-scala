package ltbs.uniform
package interpreter.asterisk

import org.asteriskjava.fastagi._

object HelloAgiScript extends BaseAgiScript {

  def service(request: AgiRequest, channel: AgiChannel): Unit = {
    answer();
    streamFile("demo-congrats");
    hangup();
  }
}

object AgiServer extends App {
  val server = new DefaultAgiServer(HelloAgiScript)
  server.startup()
}
