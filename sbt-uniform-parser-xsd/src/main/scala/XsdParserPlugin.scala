package ltbs.uniform.parsers.xsd

import sbt._
import Keys._

object UniformParserXsdPlugin extends AutoPlugin {

  val compileXsds = TaskKey[Seq[File]]("uniform-compile-xsds", "Compile XSD files into scala source files")

  override lazy val projectSettings = {
    def common = Seq(
      compileXsds := compileXsdsTask.value,
      sourceDirectories in compileXsds := Seq(sourceDirectory.value / "xsd"),
      sourceGenerators += compileXsds.taskValue
    )
    inConfig(Compile)(common) ++ inConfig(Test)(common)
  }

  def compileXsdsTask = Def.task {
    val inputFiles = (sourceDirectories in compileXsds).value.flatMap { f => 
      Option(f.listFiles).getOrElse(Array.empty).filter(_.getName.endsWith(".xsd"))
    }

    val targetDir = (target in compileXsds).value

    def convert(input: File): Seq[File] = {
      Parser.getClassesFromFile(input).map { cc =>
        val fileName = targetDir / (cc.niceName ++ ".scala")
        scala.tools.nsc.io.File(fileName).writeAll(cc.toString)
        fileName
      }
    }
      
    inputFiles.par.flatMap(convert).seq.toSeq

  }

}
