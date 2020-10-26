import sbtcrossproject.Platform
import sbt._
import sbt.Keys._

object Play25 extends Platform {
  def identifier: String = "play25"
  def sbtSuffix: String = "25"
  def enable(project: Project): Project = project
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-server"       % "2.5.19" % "provided",
        "com.typesafe.play" %% "play-omnidoc"      % "2.5.19" % "provided",
        "com.typesafe.play" %% "play-netty-server" % "2.5.19" % "provided",
        "com.typesafe.play" %% "play-logback"      % "2.5.19" % "provided",
        "com.typesafe.play" %% "play-ws"           % "2.5.19" % "provided"
      )
    )
}

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
      libraryDependencies += "com.typesafe.play" %% "play" % "2.7.5" % "provided"
    )
}

object Play28 extends Platform {
  def identifier: String = "play28"
  def sbtSuffix: String = "28"
  def enable(project: Project): Project = project
    .settings(
      libraryDependencies += "com.typesafe.play" %% "play" % "2.8.3" % "provided"
    )
}
