ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "promptly",
    idePackagePrefix := Some("digital.blueinsight.promptly")
  )

libraryDependencies += "io.cequence" %% "openai-scala-client" % "0.4.1"
libraryDependencies += ("com.typesafe.akka" %% "akka-actor" % "2.6.20").cross(CrossVersion.for3Use2_13)