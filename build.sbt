import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import AutoDocs._

scalaVersion := "3.0.0"

val allCrossScala = Seq(
  //  "2.11.12",
  "3.0.0",
  "2.12.12",
  "2.13.2"
)

lazy val root = project.in(file("."))
  .aggregate(
    coreJS,
    coreJVM,
    // `interpreter-cli`,
    `interpreter-gui`,
    interpreterLogictableJS,
    interpreterLogictableJVM,
//    `interpreter-play`.projects(Play25), // please see README.md
    `interpreter-play`.projects(Play26),
    `interpreter-play`.projects(Play27),
    `interpreter-play`.projects(Play28),    
    exampleProgramsJS,
    exampleProgramsJVM,
    commonWebJVM,
  )
  .settings(
    skip in compile := true, 
    publishLocal := {},
    publish := {},
    test := {},
    publishArtifact := false,
    publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo"))),
    organization := "com.luketebbs.uniform",
    scalaVersion := allCrossScala.find(_.startsWith("2.12")).get
  )

enablePlugins(SemVerPlugin, SiteScaladocPlugin)

def macroDependencies(scalaVersion: String) =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 =>
      Seq(
        compilerPlugin(("org.scalamacros" %% "paradise" % "2.1.1").cross(CrossVersion.patch)),
        "org.scala-lang" % "scala-reflect" % scalaVersion % Provided
      )
    case Some((2, minor)) if minor == 13 => Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion
    )
    case Some((3, _)) => Nil
  }

lazy val commonSettings = Seq(
  homepage := Some(url("https://ltbs.github.io/uniform-scala/")),
  organization := "com.luketebbs.uniform",
//  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  scalaVersion := allCrossScala.find(_.startsWith("3.0")).get, 
  crossScalaVersions := allCrossScala,
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings", "-Ywarn-unused"),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/ltbs/uniform-scala"),
      "scm:git@github.com:ltbs/uniform-scala.git"
    )
  ),
  developers := List(
    Developer(
      id            = "ltbs",
      name          = "Luke Tebbs",
      email         = "luke@luketebbs.com",
      url           = url("http://www.luketebbs.com/")
    ),
    Developer(
      id            = "mattrobertsky",
      name          = "Matt Roberts",
      email         = "matt.roberts2@digital.hmrc.gov.uk",
      url           = url("https://github.com/mattrobertsky")
    )
  ),
  publishTo := {
    sonatypePublishToBundle.value
  },
  publishConfiguration := publishConfiguration.value.withOverwrite(isSnapshot.value),
  com.typesafe.sbt.pgp.PgpKeys.publishSignedConfiguration := com.typesafe.sbt.pgp.PgpKeys.publishSignedConfiguration.value.withOverwrite(isSnapshot.value),
  publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(isSnapshot.value),
  com.typesafe.sbt.pgp.PgpKeys.publishLocalSignedConfiguration := com.typesafe.sbt.pgp.PgpKeys.publishLocalSignedConfiguration.value.withOverwrite(isSnapshot.value),
  git.gitTagToVersionNumber := { tag: String =>
    if(tag matches "[0-9]+\\..*") Some(tag)
    else None
  },
  useGpg := true,
  licenses += ("GPL-3.0", url("https://www.gnu.org/licenses/gpl-3.0.en.html")),
  libraryDependencies ++= Seq(
//    "org.scalatest" %%% "scalatest" % "3.1.2" % "test", 
//    "org.scalatestplus" %%% "scalacheck-1-14" % "3.2.0.0-M4" % "test", 
//    compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.0" cross CrossVersion.full),
//    "com.github.ghik" % "silencer-lib" % "1.7.0" % Provided cross CrossVersion.full
  )
)


lazy val treelike = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.6.1",
    ),
    initialCommands in console := List(
      "import cats.implicits._",
    ).mkString("; ")
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.6.1",
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.0.0",
//      "org.typelevel" %%% "simulacrum" % "1.0.0",
      "dev.zio" %%% "izumi-reflect" % "1.1.2",
//      "org.typelevel" %%% "cats-effect" % "2.1.3" % "test",      
    ) ++ macroDependencies(scalaVersion.value),
    initialCommands in console := List(
      "import cats.implicits._",
      "val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe",
      "import universe._"
    ).mkString("; ")
  )

lazy val coreJS = core.js.dependsOn(treelike.js)
lazy val coreJVM = core.jvm.dependsOn(treelike.jvm)

lazy val coreDocs = docProject(coreJVM, docs)

lazy val `common-web` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.propensive" %%% "magnolia" % "0.16.0",
      "org.portable-scala" %%% "portable-scala-reflect" % "1.0.0"
//      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    ) ++ macroDependencies(scalaVersion.value)
  )

lazy val commonWebJVM = `common-web`.jvm
  .dependsOn(core.jvm)

lazy val commonWebJS = `common-web`.js
  .dependsOn(core.js)

lazy val commonWebDocumentation = docProject(commonWebJVM, docs)

lazy val `interpreter-cli` = project
  .settings(commonSettings)
  .dependsOn(coreJVM)
  .dependsOn(exampleProgramsJVM % "test")

lazy val `interpreter-gui` = project
  .settings(commonSettings)
  .dependsOn(coreJVM)

lazy val `interpreter-logictable` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)

lazy val interpreterLogictableJS = `interpreter-logictable`.js
  .dependsOn(coreJS)
  .dependsOn(exampleProgramsJS % "test")

lazy val interpreterLogictableJVM = `interpreter-logictable`.jvm
  .dependsOn(coreJVM)
  .dependsOn(exampleProgramsJVM % "test")

lazy val interpreterLogictableDocs = docProject(interpreterLogictableJVM, docs)

lazy val `interpreter-play`: sbtcrossproject.CrossProject =
  crossProject(Play25, Play26, Play27, Play28)
    .crossType(CrossType.Full)
    .settings(commonSettings)
    .configure(_.dependsOn(core.jvm, `common-web`.jvm))
    // .configurePlatform(Play25) {_.settings(
    //   name := "interpreter-play25",
    //   scalaVersion := allCrossScala.find(_.startsWith("2.11")).get,
    //   crossScalaVersions := allCrossScala.filter{_.startsWith("2.11")}
    // )}
    .configurePlatform(Play26) {_.settings(
      name := "interpreter-play26",      
      crossScalaVersions := allCrossScala.filter{x => x.startsWith("2.11") || x.startsWith("2.12")}
    )}
    .configurePlatform(Play27) {_.settings(
      name := "interpreter-play27",            
    )}
    .configurePlatform(Play28) {_.settings(
      name := "interpreter-play28",            
      crossScalaVersions := allCrossScala.filterNot{x => x.startsWith("2.11")}
    )}

lazy val `interpreter-play27` = `interpreter-play`.projects(Play27)
  .dependsOn(commonWebJVM)
  .dependsOn(exampleProgramsJVM % "test")

lazy val `interpreter-play-docs` = docProject(`interpreter-play27`, docs)

lazy val `interpreter-js` = project
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.querki" %%% "jquery-facade" % "2.0"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(commonWebJS)

lazy val `interpreter-js-docs` = docProject(`interpreter-js`, docs)

lazy val `example-programs` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .dependsOn(core)

lazy val exampleProgramsJS = `example-programs`.js.dependsOn(coreJS)
lazy val exampleProgramsJVM = `example-programs`.jvm.dependsOn(coreJVM)

lazy val `example-assets` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.9.1"
  )

lazy val exampleAssetsJS = `example-assets`.js.dependsOn(`common-web`.js)
lazy val exampleAssetsJVM = `example-assets`.jvm.dependsOn(`common-web`.jvm)

lazy val `example-play` = project.settings(commonSettings)
  .enablePlugins(PlayScala)
  .dependsOn(
    `interpreter-play`.projects(Play26),
    core.jvm,
    `example-programs`.jvm,
    `example-assets`.jvm
  )
  .settings(
//    scalacOptions += "-Xprint:typer",
    TwirlKeys.templateImports ++= Seq(
      "ltbs.uniform._",
      "ltbs.uniform.interpreters.playframework._"
    ),
    PlayKeys.playDefaultPort := 9001,
    libraryDependencies ++= Seq(
      filters,
      guice
    ),
    initialCommands in console := "import cats.implicits._; import ltbs.uniform._; import ltbs.uniform.interpreters.playframework._",
    initialCommands in consoleQuick := """import cats.implicits._;""",
    scalacOptions -= "-Xfatal-warnings", // twirl....
    crossScalaVersions ~= {_.filter{_.startsWith("2.12")}}
  )

lazy val `example-js` = project
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    crossScalaVersions ~= {_.filter{_.startsWith("2.12")}},    
    libraryDependencies ++= Seq(
      "org.querki" %%% "jquery-facade" % "2.0", 
      "org.scala-js" %%% "scalajs-java-time" % "1.0.0",
      "com.lihaoyi" %%% "scalatags" % "0.9.1"
    )
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(
    `interpreter-js`,
    exampleProgramsJS,
    `example-assets`.js    
  )

lazy val docs = project
  .enablePlugins(MicrositesPlugin)
  .settings(commonSettings)
  .settings(
    fork in Test := true,
    micrositeName           := "uniform-scala",
    micrositeDescription    := "Purely functional user-interaction",
    micrositeAuthor         := "Luke Tebbs",
    micrositeGithubOwner    := "ltbs",
    micrositeGithubRepo     := "uniform-scala",
    micrositeBaseUrl        := "/uniform-scala",
    micrositeGitterChannel  := false,
    micrositeHighlightTheme := "color-brewer",
    micrositeConfigYaml     := microsites.ConfigYml(yamlCustomProperties = Map(
      "last-stable-version" -> com.typesafe.sbt.SbtGit.GitKeys.gitDescribedVersion.value.fold("")(_.takeWhile(_ != '-'))
    )),
    micrositePalette := Map(
      "brand-primary"   -> "#5236E0",
      "brand-secondary" -> "#32423F",
      "brand-tertiary"  -> "#232F2D",
      "gray-dark"       -> "#3E4645",
      "gray"            -> "#7F8483",
      "gray-light"      -> "#E2E3E3",
      "gray-lighter"    -> "#F3F4F4",
      "white-color"     -> "#FFFFFF"),
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play" % "2.6.20", // used for the play interpreter demo
      "org.scalatest" %%% "scalatest" % "3.0.5" // used to demo unit tests from logictables
    ),
    // makeMicrosite := (makeMicrosite
    //   dependsOn (coreDocs / mdoc)
    //   dependsOn mdoc.in(`interpreter-logictable-docs`)
    //   dependsOn mdoc.in(`interpreter-play-docs`)
    //   dependsOn mdoc.in(`interpreter-cli-docs`)
    // ).value
)
