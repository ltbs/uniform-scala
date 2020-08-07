resolvers ++= Seq(
  Resolver.bintrayIvyRepo("rallyhealth", "sbt-plugins"), 
  Resolver.jcenterRepo
)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.33")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.2")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("com.rallyhealth.sbt" % "sbt-git-versioning" % "1.4.0")

addSbtPlugin("com.47deg"  % "sbt-microsites" % "1.2.1")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.4")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.8.2")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.5.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")

// ========================================
// https://github.com/sbt/sbt/issues/5107
resolvers += "Sonatype OSS Staging" at "https://oss.sonatype.org/content/repositories/staging"
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.7.0") 
// ========================================
