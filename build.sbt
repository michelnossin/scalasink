name := "training"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies +=
  "com.typesafe.akka" % "akka-actor_2.11" % "2.4-SNAPSHOT"

libraryDependencies <++= scalaVersion(v =>
  Seq("org.scala-lang" % "scala-actors" % v)
)

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"