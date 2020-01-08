package controllers

import play.api.http.{Writeable, ContentTypeOf, ContentTypes}
import play.api.mvc.Codec
import scalatags.Text.all._

object ScalatagsSupport {

  implicit def contentTypeOfTag(implicit codec: Codec): ContentTypeOf[Tag] = {
    ContentTypeOf[Tag](Some(ContentTypes.HTML))
  }

  implicit def writeableOfTag(implicit codec: Codec): Writeable[Tag] = {
    Writeable(tag => codec.encode("<!DOCTYPE html>\n" + tag.render))
  }

}
