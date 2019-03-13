package ltbs.uniform.gformsparser

import sbt._
import Keys._
import pureconfig._

object GformToUniformPlugin extends AutoPlugin {

  val gformsToUniform = TaskKey[Seq[File]]("gforms-to-uniform", "Compile gform JSON into uniform source files")
  val gformsAddressClass = TaskKey[Option[String]]("gforms-address-class", "fully qualified class to be used for the addresses in converted gforms, if empty the journey will define its own address")

  val gformsJourneyPackage = TaskKey[String]("gforms-journey-package", "package under which generated journey are created")
  val gformsControllerPackage = TaskKey[Option[String]]("gforms-controller-package", "package under which play journey controllers are created, if None then they are omitted")
  val gformsLogicTableTests = TaskKey[Boolean]("gforms-logic-table-tests", "If true then unit tests based upon logic tables should be produced")

  override lazy val projectSettings = {
    def common = Seq(
      gformsToUniform := gformsToUniformTask.value,
      sourceDirectories in gformsToUniform := Seq(sourceDirectory.value / "gform"),
      sourceGenerators += gformsToUniform.taskValue,
      managedSourceDirectories += (target in gformsToUniform).value,
      gformsAddressClass := None,
      gformsJourneyPackage := "gforms",
      gformsControllerPackage := None,       
      gformsLogicTableTests := false, 
      includeFilter in gformsToUniform := "*.json",
      sources in gformsToUniform := Defaults.collectFiles(
        sourceDirectories in gformsToUniform,
        includeFilter in gformsToUniform,
        excludeFilter in gformsToUniform
      ).value,
      watchSources in Defaults.ConfigGlobal ++= (sources in gformsToUniform).value,
      target in gformsToUniform := crossTarget.value / "gform" / Defaults.nameForSrc(configuration.value.name)
    )

    inConfig(Compile)(common) ++ inConfig(Test)(common)
  }

  object Colours {
    val black="\u001b[30m"
    val red="\u001b[31m"
    val green="\u001b[32m"
    val yellow="\u001b[33m"
    val blue="\u001b[34m"
    val magenta="\u001b[35m"
    val cyan="\u001b[36m"
    val white="\u001b[37m"
    val reset="\u001b[0m"
  }

  def gformsToUniformTask = Def.task {
    val inputFiles = (sourceDirectories in gformsToUniform).value.flatMap { f =>
      Option(f.listFiles).getOrElse(Array.empty).filter(_.getName.endsWith(".json"))
    }

    val targetDir = (target in gformsToUniform).value

    val config = Config(
      gformsAddressClass.value,
      gformsJourneyPackage.value,
      gformsControllerPackage.value,
      gformsLogicTableTests.value
    )

    def writeOut(sourceMod: Long, target: File, code: String): Unit = {
      import Colours._

      if (!target.getParentFile.exists) target.getParentFile.mkdirs      
      val newFile = !target.exists
      val modified = target.exists && target.lastModified < sourceMod
      if (newFile || modified) {
        if (newFile) {
          println(s"  ${blue}$target${reset} being generated${reset}")
        } else {
          println(s"  ${blue}$target${reset} being updated${reset}")
        }
        scala.tools.nsc.io.File(target).writeAll(code)
      }
    }

    def convert(input: File): Seq[File] = {
      import Colours._

      val target = targetDir / (input.getName.replaceFirst("\\.json$",".scala"))
      val controllerTarget = targetDir / (input.getName.replaceFirst("\\.json$","Controller.scala"))
      val newFile = !target.exists
      val modified = target.exists && target.lastModified < input.lastModified
      if (newFile || modified) {
        println(s"${green}GFORMS: processing ${blue}${input}${reset}")
        val (journeyCode,controllerOpt,testOpt) = TemplateToSourceCode(readFile(input),config)
        writeOut(input.lastModified, target, journeyCode)


        controllerOpt.map{ controllerCode =>
          writeOut(input.lastModified, controllerTarget, controllerCode)
        }

        testOpt.map{ testCode =>

        }        
      }

      List(
        Some(target),
        Some(controllerTarget).filter(_ => config.controllerPackage.isDefined)
      ).flatten
    }

    inputFiles.par.flatMap(convert).seq.toSeq
  }

  def readFile(input: File): GformTemplate = {
    implicit def hint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))
    implicit val fieldHint = new FieldCoproductHint[Field]("type") {
      override def fieldValue(name: String) = name.dropRight("Field".length).toLowerCase
      // some of the gform schemas don't have a 'type' value at all.
      // this is in violation of their own documentation.
      // consider overriding some methods here to default to 'text'
    }

    loadConfigFromFiles[GformTemplate](List(input.toPath), true) match {
      case Right(gformsTemplate) =>
        gformsTemplate
      case Left(err) =>
        throw new IllegalStateException(err.toString)
    }
  }
}
