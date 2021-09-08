import sbt._, Keys._
import mdoc._, MdocPlugin.autoImport._

object AutoDocs { 
  def docProject(mainProject: Project, docs: Project): Project = {

    val isCrossBuild = mainProject
      .base
      .getAbsolutePath.split("/")
      .last match {
        case "js" | "jvm" => true
        case x if x.startsWith("play") || x.startsWith(".") => true
        case _ => false
      }

    val baseDir = if (isCrossBuild)
      (mainProject.base / "..").getCanonicalFile
    else
      mainProject.base

    val d = Project(mainProject.id + "Documentation", mainProject.base / "docs")
      .enablePlugins(MdocPlugin)
      .settings(
        scalacOptions := (mainProject / scalacOptions).value,
        mdocIn := baseDir / "docs",
        mdocOut := (docs/baseDirectory).value / "docs" / mainProject.id
      )
      .dependsOn(mainProject)
    d
  }
}
