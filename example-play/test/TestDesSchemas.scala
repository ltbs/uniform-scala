package controllers

import org.scalatest._, flatspec.AnyFlatSpec, matchers.should.Matchers
import play.api.libs.json._
class TestDesSchemas extends AnyFlatSpec with Matchers {

  val dir = new java.io.File("conf/dst")
  dir.listFiles().filter{_.getName.contains("example")}.sortBy(_.getName) map { file =>
    val schemaName = file.getAbsolutePath().replaceAll("example[0-9]","schema")
    val schemaFile = new java.io.File(schemaName)
    s"${file.getName}" should s"have a schema called ${schemaFile.getName}" in {
      schemaFile.exists shouldBe (true)
    }

    it should s"conform to its schema" in {
      val is = new java.io.FileInputStream(schemaFile)
      val checker = SchemaChecker(is)
      val example = scala.io.Source.fromFile(file).getLines().mkString
      val result = checker.errorsIn(Json.parse(example))
      is.close
      result shouldBe (None)
    } 
  }
}
