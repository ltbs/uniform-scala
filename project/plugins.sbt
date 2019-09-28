resolvers ++= Seq(
  Resolver.bintrayIvyRepo("rallyhealth", "sbt-plugins"), 
  Resolver.jcenterRepo
)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.29")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "1.0.0")

addSbtPlugin("com.rallyhealth.sbt" % "sbt-git-versioning" % "1.2.1")

addSbtPlugin("com.47deg"  % "sbt-microsites" % "0.9.6")

addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.6.12")

addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.7.3")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "0.6.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-twirl" % "1.4.2")
