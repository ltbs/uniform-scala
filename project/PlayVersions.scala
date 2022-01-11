import sbtcrossproject.Platform
import sbt._
import sbt.Keys._

object Play26 extends Platform {
  def identifier: String = "play26"
  def sbtSuffix: String = "26"
  def enable(project: Project): Project = project
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.6.25" % "provided"
    )
}

object Play27 extends Platform {
  def identifier: String = "play27"
  def sbtSuffix: String = "27"
  def enable(project: Project): Project = project
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.7.9" % "provided"
    )
}

object Play28 extends Platform {
  def identifier: String = "play28"
  def sbtSuffix: String = "28"
  def enable(project: Project): Project = project
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.8.12" % "provided"
    )
}
