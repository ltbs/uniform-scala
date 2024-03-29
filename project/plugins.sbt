resolvers ++= Seq(
  Resolver.bintrayIvyRepo("rallyhealth", "sbt-plugins"), 
  Resolver.jcenterRepo
)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.15.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.10.0")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.1.1")

addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.1")

addSbtPlugin("com.rallyhealth.sbt" % "sbt-git-versioning" % "1.6.0")

addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.4.4")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.8")

addSbtPlugin("com.github.sbt" % "sbt-ghpages" % "0.8.0")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.9.2")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

addSbtPlugin("com.typesafe.play" % "sbt-twirl" % "1.6.5")

// addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.7.2")

// ========================================
// https://github.com/sbt/sbt/issues/5107
resolvers += "Sonatype OSS Staging" at "https://oss.sonatype.org/content/repositories/staging"
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.9.2") 
// ========================================
