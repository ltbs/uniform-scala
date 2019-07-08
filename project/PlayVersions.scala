import sbtcrossproject.Platform
import sbt._
import sbt.Keys._

object Play25 extends Platform {
  def identifier: String = "play25"
  def sbtSuffix: String = "25"
  def enable(project: Project): Project = project
    .settings(
      scalaVersion := "2.11.12",
      crossScalaVersions := Seq("2.11.12"),
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-server"       % "2.5.18" % "provided",
        "com.typesafe.play" %% "play-omnidoc"      % "2.5.18" % "provided",
        "com.typesafe.play" %% "play-netty-server" % "2.5.18" % "provided",
        "com.typesafe.play" %% "play-logback"      % "2.5.18" % "provided",
        "com.typesafe.play" %% "play-ws"           % "2.5.18" % "provided"
      )
    )
}

object Play26 extends Platform {
  def identifier: String = "play26"
  def sbtSuffix: String = "26"
  def enable(project: Project): Project = project
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.6.20" % "provided"
    )
}

object Play27 extends Platform {
  def identifier: String = "play27"
  def sbtSuffix: String = "27"
  def enable(project: Project): Project = project
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.7.2" % "provided"
    )
}
