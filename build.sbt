import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import AutoDocs._

val allCrossScala = Seq(
//  "2.11.12",
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
    `interpreter-js`,
    exampleProgramsJS,
    exampleProgramsJVM,
    commonWebJVM,
  )
  .settings(
    compile / skip := true,
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
    case _ => Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion
    )
  }

lazy val commonSettings = Seq(
  homepage := Some(url("https://ltbs.github.io/uniform-scala/")),
  organization := "com.luketebbs.uniform",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  scalaVersion := allCrossScala.find(_.startsWith("2.12")).get,
  crossScalaVersions := allCrossScala,
  scalacOptions ++= Seq(
//    "-P:silencer:checkUnused",           // silencer plugin to fail build if supressing a non-existant warning
//    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inacces(sible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  ) ++ {CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2,11)) => Seq(
      "-Xfuture",                          // Turn on future language features.
      "-Ywarn-unused",
      "-Ypartial-unification",             // Enable partial unification in type constructor inference
      "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
      "-Ywarn-inaccessible"               // Warn about inaccessible types in method signatures.
    )
    case Some((2,12)) => Seq(
      "-Xfuture",                          // Turn on future language features.
      "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
      "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
      "-Xlint:unsound-match",              // Pattern match may not be typesafe.
      "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
      "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
      "-Ywarn-unused:locals",              // Warn if a local definition is unused.
      "-Ywarn-unused:params",              // Warn if a value parameter is unused.
      "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
      "-Ywarn-unused:privates",            // Warn if a private member is unused.
      "-Ywarn-extra-implicit",              // Warn when more than one implicit parameter section is defined.
      "-Ypartial-unification",             // Enable partial unification in type constructor inference
      "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
      "-Ywarn-inaccessible"               // Warn about inaccessible types in method signatures.
    )
    case Some((2,13)) => Seq(
      "-Ymacro-annotations"
    )
    case _ => Nil
  }},
  Compile / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings", "-Ywarn-unused"),
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
    "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test,
    compilerPlugin("com.github.ghik" % "silencer-plugin" % "1.7.0" cross CrossVersion.full),
    "com.github.ghik" % "silencer-lib" % "1.7.0" % Provided cross CrossVersion.full
  )
)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % (if (scalaVersion.value.startsWith("2.11")) "2.0.0" else "2.6.1"),
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.2",
      "org.typelevel" %%% "simulacrum" % "1.0.1",
      "dev.zio" %%% "izumi-reflect" % "2.0.1",
      "org.typelevel" %%% "cats-effect" % (if (scalaVersion.value.startsWith("2.11")) "2.0.0" else "3.2.8" )  % "test"
    ) ++ macroDependencies(scalaVersion.value),
    console / initialCommands := List(
      "import cats.implicits._",
      "val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe",
      "import universe._"
    ).mkString("; ")
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val coreDocs = docProject(coreJVM, docs)

lazy val `common-web` = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Full)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.propensive" %%% "magnolia" % (if (scalaVersion.value.startsWith("2.11")) "0.10.0" else "0.17.0" ),
      "org.portable-scala" %%% "portable-scala-reflect" % "1.1.1"
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
  .settings( libraryDependencies ++= macroDependencies(scalaVersion.value))

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

lazy val `interpreter-play-docs` = docProject(`interpreter-play27`, docs).settings{
  libraryDependencies += "com.typesafe.play" %% "play" % "2.7.9"
}

lazy val `interpreter-js` = project
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.querki" %%% "jquery-facade" % "2.0",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
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
    console / initialCommands := "import cats.implicits._; import ltbs.uniform._; import ltbs.uniform.interpreters.playframework._",
    consoleQuick / initialCommands := """import cats.implicits._;""",
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
    Test / fork := true,
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
//      "org.scalatest" %%% "scalatest" % "3.0.5" // used to demo unit tests from logictables
    ),
    // makeMicrosite := (makeMicrosite
    //   dependsOn (coreDocs / mdoc)
    //   dependsOn mdoc.in(`interpreter-logictable-docs`)
    //   dependsOn mdoc.in(`interpreter-play-docs`)
    //   dependsOn mdoc.in(`interpreter-cli-docs`)
    //).value
)
