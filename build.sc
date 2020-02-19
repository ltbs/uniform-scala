// build.sc
import mill._, scalalib._, mill.scalalib.publish._, mill.scalajslib._

trait UniformModule extends Module with PublishModule with CrossScalaModule {

  def publishVersion = "0.0.0"

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.luketebbs.uniform",
    url = "https://ltbs.github.io/uniform-scala/",
    licenses = Seq(License.`GPL-3.0+`),
    versionControl = VersionControl.github("ltbs", "uniform-scala"),
    developers = Seq(
      Developer(
        id            = "ltbs",
        name          = "Luke Tebbs",
        url           = "http://www.luketebbs.com/"
      ),
      Developer(
        id            = "mattrobertsky",
        name          = "Matt Roberts",
        url           = "https://github.com/mattrobertsky"
      )
    )
  )

  def scalacPluginIvyDeps = (if (crossScalaVersion.startsWith("2.13")) {
    super.scalacPluginIvyDeps()    
  } else {
    super.scalacPluginIvyDeps() ++ Agg(
      ivy"com.github.ghik::silencer-plugin:1.4.2",      
      ivy"org.scalamacros:paradise_${crossScalaVersion}:2.1.1"
    )
  }) ++ Agg(
      ivy"org.typelevel:kind-projector_${crossScalaVersion}:0.11.0"
  )

  def scalacOptions = {
    Seq(
      "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
      "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
      "-encoding", "utf-8",                // Specify character encoding used by source files.
      "-explaintypes",                     // Explain type errors in more detail.
      "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
      "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
      "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
      "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
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
      "-Ywarn-dead-code",                  // Warn when dead code is identified.
      "-Ywarn-numeric-widen",              // Warn when numerics are widened.
      "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
    ) ++ {crossScalaVersion match {
      case x if x.startsWith("2.11") => Seq(
        "-Xfuture",                          // Turn on future language features.
        "-Ywarn-unused",
        "-Ypartial-unification",             // Enable partial unification in type constructor inference
        "-Ywarn-infer-any",                  // Warn when a type argument is inferred to be `Any`.
        "-Ywarn-nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
        "-Ywarn-nullary-unit",               // Warn when nullary methods return Unit.
        "-Ywarn-inaccessible"               // Warn about inaccessible types in method signatures.
      )
      case x if x.startsWith("2.12") => Seq(
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
      case x if x.startsWith("2.13") => Seq(
        "-Ymacro-annotations"
      )
      case _ => Nil
    }}
  }

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.github.ghik::silencer-lib:1.4.2" // TODO: Provided
  )

  lazy val parent = this

  object test extends Tests{

    def ivyDeps = Agg(
      ivy"org.scalatestplus::scalatestplus-scalacheck:3.1.0.0-RC2"
    )
    def testFrameworks = Seq("org.scalatest.tools.Framework")

    def moduleDeps = super.moduleDeps ++ Seq(parent, `example-programs`(crossScalaVersion))

    def sources = T.sources(
      parent.millSourcePath / "test"
    )
    
  }
}

class ExamplePrograms(val crossScalaVersion: String) extends Module with CrossSbtModule {
   def moduleDeps = super.moduleDeps ++ Seq(core("jvm", crossScalaVersion))
}
object `example-programs` extends Cross[ExamplePrograms]("2.11.12", "2.12.10", "2.13.1")

class Core(val platformSegment: String, val crossScalaVersion: String) extends UniformModule {
  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"org.typelevel::cats-core:2.0.0",
    ivy"org.scala-lang.modules::scala-parser-combinators:1.1.2",
    ivy"com.chuusai::shapeless:2.3.3",
    ivy"org.typelevel::simulacrum:1.0.0" // TODO: Provided
  )

  override def sources = {
    def shortCrossVersion = crossScalaVersion.replaceAll("\\.[0-9]+$", "")
    T.sources(
      millSourcePath / os.up / s"src",      
      millSourcePath / os.up / s"src-$shortCrossVersion",
      millSourcePath / os.up / s"src-$platformSegment",
      millSourcePath / os.up / s"src-${platformSegment}-${shortCrossVersion}"
    )
  }

}

implicit def v = define.Cross.Factory[Core]{
  case (("jvm", crossScalaVersion: String), ctx) => new Core("jvm", crossScalaVersion) {
    override def millOuterCtx = ctx
  }
  case (("js", crossScalaVersion: String), ctx) => new Core("js", crossScalaVersion) with ScalaJSModule {
    override def millOuterCtx = ctx
    def scalaJSVersion = "1.0.0"
  }
}

object core extends Cross[Core](
  ("jvm", "2.11.12"),
  ("jvm", "2.12.10"),
  ("jvm", "2.13.1"),
  ("js", "2.13.1")
)

class CommonWeb(val crossScalaVersion: String) extends UniformModule {
  def moduleDeps = Seq(core("jvm", crossScalaVersion))
}

object `common-web` extends Cross[CommonWeb]("2.11.12", "2.12.10", "2.13.1")

class InterpreterLogicTable(val crossScalaVersion: String) extends UniformModule {
  def moduleDeps = Seq(core("jvm", crossScalaVersion))
}
object `interpreter-logictable` extends Cross[InterpreterLogicTable]("2.11.12", "2.12.10", "2.13.1")

class InterpreterCli(val crossScalaVersion: String) extends UniformModule {
  def moduleDeps = Seq(core("jvm", crossScalaVersion))
}
object `interpreter-cli` extends Cross[InterpreterCli]("2.11.12", "2.12.10", "2.13.1")

class InterpreterPlay(val playVersion: String, val crossScalaVersion: String) extends UniformModule {
  def moduleDeps = Seq(`common-web`(crossScalaVersion))

  def ivyDeps = super.ivyDeps() ++ {
    if (playVersion.startsWith("2.5")) Agg(
      ivy"com.typesafe.play::play-server:${playVersion}",
      ivy"com.typesafe.play::play-omnidoc:${playVersion}",
      ivy"com.typesafe.play::play-netty-server:${playVersion}",
      ivy"com.typesafe.play::play-logback:${playVersion}",
      ivy"com.typesafe.play::play-ws:${playVersion}"
    ) else Agg(
      ivy"com.typesafe.play::play:2.7.4"
    )
  }

}

object `interpreter-play` extends Cross[InterpreterPlay](
  ("2.5.19", "2.11.12"),
  ("2.6.25", "2.11.12"),
  ("2.6.25", "2.12.10"),
  ("2.7.4",  "2.11.12"),
  ("2.7.4",  "2.12.10"),
  ("2.7.4",  "2.13.1"),
  ("2.8.0",  "2.12.10"),
  ("2.8.0",  "2.13.1")
)

