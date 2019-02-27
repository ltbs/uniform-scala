package ltbs.uniform.gformsparser

import collection.mutable.Stack
import org.scalatest._

class GformDecoderSpec extends FunSpec with Matchers {
  import GformDecoderSpec._
  describe("GformDecoder") {

    for (file <- files) {
      val decoded = GformDecoder(file.getAbsolutePath)
      it(s"should parse $file ") {
        decoded should be ('right)
      }
    }
  }
}

class ParserSpec extends FunSpec with Matchers {
  import GformDecoderSpec._
  describe("Parser") {

    val pwd = System.getenv("PWD")
    println(pwd)
    it(s"should compile gforms-parser/src/test/resources/ofsted-sc1.json") {
      """Parser.parseGform("gforms-parser/src/test/resources/ofsted-sc1.json")""" should compile
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
