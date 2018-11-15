package ltbs.uniform.gformsparser

import collection.mutable.Stack
import org.scalatest._

class GformDecoderSpec extends FunSpec with Matchers {
  import GformDecoderSpec._
  describe("GformDecoder") {

    for (file <- files) {
        it(s"should parse $file ") {
          val decoded = GformDecoder(file.getAbsolutePath)
          decoded should be ('right)
        }
    }

  }
}

object GformDecoderSpec{
  val files = {
    val base =new java.io.File(System.getenv("HOME") ++ "/hmrc/gform-templates")
    if (!base.exists){
      throw new java.io.FileNotFoundException(s"Expecting gforms template repo at ${base.getAbsolutePath}")
    }

    base.listFiles.toList.filter(_.getName.endsWith(".json"))
  }

  val data = files.map{ x => 
    (GformDecoder(x.getAbsolutePath))
  }

}
