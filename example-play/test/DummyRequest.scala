import play.api.mvc._
import play.api.libs.typedmap.TypedMap
import java.net.InetAddress.getLocalHost
import ltbs.uniform.web.FormUrlEncoded

package object playexample {

  protected def dummyHeader(methodIn: String, pathIn: String) = new RequestHeader {
    val attrs = TypedMap.empty
    val connection = request.RemoteConnection(getLocalHost, false, None)
    val headers = new Headers(Nil)
    val method = methodIn
    val target = request.RequestTarget(pathIn,"/" + pathIn.split("/").last,Map.empty)
    val version = ""
  }

  def dummyGet(path: String) = Request(dummyHeader("get",path), AnyContent())

  def dummyPost(path: String, payload: FormUrlEncoded) =
    Request(
      dummyHeader("post",path),
      AnyContent(payload)
    )

}

