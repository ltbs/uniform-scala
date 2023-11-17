resolvers ++= Seq(
  Resolver.bintrayIvyRepo("rallyhealth", "sbt-plugins"), 
  Resolver.jcenterRepo
)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.10.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.13")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2-1")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.2")

addSbtPlugin("com.rallyhealth.sbt" % "sbt-git-versioning" % "1.6.0")

addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.3.4")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.3")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.8.16")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

addSbtPlugin("com.typesafe.play" % "sbt-twirl" % "1.6.3")

// addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.7.2")

// ========================================
// https://github.com/sbt/sbt/issues/5107
resolvers += "Sonatype OSS Staging" at "https://oss.sonatype.org/content/repositories/staging"
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.9.2") 
// ========================================
