import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}
import microsites.ExtraMdFileConfig

lazy val root = project.in(file("."))
  .aggregate(
    coreJS,
    coreJVM,
    `interpreter-cli`,
    `interpreter-gui`,
    interpreterLogictableJS,
    interpreterLogictableJVM,
//    `interpreter-play25`,
    `interpreter-play26`,
    `interpreter-js`,
    exampleProgramsJS,
    exampleProgramsJVM, 
    commonWebJS,
    commonWebJVM,
    govukWidgetsJS,
    govukWidgetsJVM
  )
  .settings(
    publishLocal := {},
    publish := {},
    publishArtifact := false
  )
  .settings(commonSettings)

enablePlugins(GitVersioning)

lazy val commonSettings = Seq(
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.11.12", "2.12.8"),
  homepage := Some(url("https://ltbs.github.io/uniform-scala/")),
  organization := "com.luketebbs.uniform",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  scalacOptions ++= Seq(
//    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfuture",                          // Turn on future language features.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative",  // By-name parameter of right associative operator.
//    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
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
    "-Xlint:unsound-match",              // Pattern match may not be typesafe.
    "-Yno-adapted-args",                 // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification",             // Enable partial unification in type constructor inference
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
//    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible",               // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused",
//    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
//    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
//   "-Ywarn-unused:locals",              // Warn if a local definition is unused.
//    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
//    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
//    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
  ),
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
    )
  ),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
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
  licenses += ("GPL-3", url("https://www.gnu.org/licenses/gpl-3.0.en.html")),
  libraryDependencies ++= Seq(
    "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
  )
)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    libraryDependencies ++= Seq(
      "org.atnos" %%% "eff" % "5.2.0",
      "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
    ),
    scalaJSUseMainModuleInitializer := true
  )

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val `common-web` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .settings(commonSettings)
  .settings(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.3.3",
      "com.github.mpilquist" %%% "simulacrum" % "0.14.0",
      "com.typesafe.play" %%% "twirl-api" % "1.3.15",
      "com.beachape" %%% "enumeratum" % "1.5.13",
      "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "1.1.1"
    )
  )

lazy val commonWebJVM = `common-web`.jvm.dependsOn(coreJVM)
lazy val commonWebJS = `common-web`.js.dependsOn(coreJS)

lazy val `interpreter-cli` = project
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8")
  )
  .dependsOn(coreJVM)

lazy val `interpreter-gui` = project
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8")
  )
  .dependsOn(coreJVM)

lazy val `interpreter-logictable` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8")
  )

lazy val interpreterLogictableJS = `interpreter-logictable`.js
  .dependsOn(coreJS)
  .dependsOn(exampleProgramsJS % "test")

lazy val interpreterLogictableJVM = `interpreter-logictable`.jvm
  .dependsOn(coreJVM)
  .dependsOn(exampleProgramsJVM % "test")

lazy val `interpreter-play`: sbtcrossproject.CrossProject =
  crossProject(Play25, Play26)
    .crossType(CrossType.Full)
    .settings(commonSettings)
    .configurePlatform(Play25)(_.settings(
      name := "interpreter-play25",
      scalaVersion := "2.11.12",
      crossScalaVersions := Seq("2.11.12")
    ))
    .configurePlatform(Play26)(_.settings(
      name := "interpreter-play26"
    ))

// lazy val `interpreter-play25` = `interpreter-play`.projects(Play25)
//   .dependsOn(commonWebJVM)

lazy val `interpreter-play26` = `interpreter-play`.projects(Play26)
  .dependsOn(commonWebJVM)

lazy val `interpreter-js` = project
  .settings(commonSettings)
  .enablePlugins(TutPlugin)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8")
  )
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "org.querki" %%% "jquery-facade" % "1.2"
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(commonWebJS, `exampleProgramsJS` % Tut)

lazy val `example-programs` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    libraryDependencies += "com.beachape" %%% "enumeratum" % "1.5.13"
  )

lazy val exampleProgramsJS = `example-programs`.js.dependsOn(coreJS)
lazy val exampleProgramsJVM = `example-programs`.jvm.dependsOn(coreJVM)

lazy val `example-play` = project.settings(commonSettings)
  .enablePlugins(PlayScala)
  .dependsOn(`interpreter-play26`, exampleProgramsJVM, commonWebJVM, govukWidgetsJVM)
  .dependsOn(interpreterLogictableJVM % "test")
  .settings(
    libraryDependencies ++= Seq(
      filters,
      guice
    ),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % "test",
    initialCommands in console := "import cats.implicits._; import ltbs.uniform._; import ltbs.uniform.web._; import ltbs.uniform.web.parser._; import ltbs.uniform.interpreters.playframework._; import ltbs.uniform.sampleprograms.BeardTax._; import ltbs.uniform.widgets.govuk._; implicit val messages: Messages = NoopMessages",
    initialCommands in consoleQuick := """import cats.implicits._;"""
  )

lazy val `example-js` = project
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.querki" %%% "jquery-facade" % "1.2",
      "org.scala-js" %%% "scalajs-java-time" % "0.2.5"
    )
  )
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(`interpreter-js`, exampleProgramsJS, govukWidgetsJS)

lazy val `govuk-widgets` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
    crossScalaVersions := Seq("2.11.12", "2.12.8"),
    libraryDependencies ++= Seq(
      "com.chuusai" %%% "shapeless" % "2.3.3",
      "com.beachape" %%% "enumeratum" % "1.5.13",
      "org.scalatest" %%% "scalatest" % "3.0.5" % "test"
    ),
    sourceDirectories in (Compile, TwirlKeys.compileTemplates) := (unmanagedSourceDirectories in Compile).value,
    TwirlKeys.templateImports ++= Seq(
      "ltbs.uniform.web._",
      "ltbs.uniform._",      
      "ltbs.uniform.widgets.govuk._"
    ),
    initialCommands in console := "import ltbs.uniform._;import ltbs.uniform.widgets.govuk._;import ltbs.uniform.datapipeline._",
    scalacOptions --= Seq(
      "-Ywarn-unused:imports",
      "-Ywarn-unused-imports",      
      "-Xfatal-warnings",
      "-Ywarn-unused",
      "-Ywarn-unused:params"
    ) // little we can do about twirl throwing warnings
  )
  .enablePlugins(SbtTwirl)

lazy val govukWidgetsJVM = `govuk-widgets`.jvm.dependsOn(commonWebJVM)
lazy val govukWidgetsJS = `govuk-widgets`.js.dependsOn(commonWebJS)

lazy val docs = project
  .dependsOn(coreJVM, `interpreter-play26`, interpreterLogictableJVM, `interpreter-cli`, exampleProgramsJVM)
  .aggregate(`interpreter-js`)
  .enablePlugins(MicrositesPlugin)
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.12.8",
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
    micrositeExtraMdFiles := Map(
      file("interpreter-js/target/scala-2.12/tut/interpreter-js.md") -> ExtraMdFileConfig(
        "interpreter-js.md",
        "docs"
      )
    ),
    scalacOptions in Tut --= Seq("-Ywarn-unused-import", "-Ywarn-unused:imports", "-Ywarn-unused"),
//    scalacOptions in Tut += "-Xfatal-warnings", // play controller scuppers this
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play" % "2.6.20", // used for the play interpreter demo
      "org.scalatest" %%% "scalatest" % "3.0.5" // used to demo unit tests from logictables
    )
  )
