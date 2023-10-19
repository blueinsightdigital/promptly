ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "promptly",
    idePackagePrefix := Some("digital.blueinsight.promptly")
  )

libraryDependencies += "io.cequence" %% "openai-scala-client" % "0.4.1"
libraryDependencies += ("com.typesafe.akka" %% "akka-actor" % "2.6.20").cross(CrossVersion.for3Use2_13)
libraryDependencies += "com.lihaoyi" %% "cask" % "0.9.1"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.12.0"
libraryDependencies += "io.circe" %% "circe-core" % "0.14.1"
libraryDependencies += "io.circe" %% "circe-generic" % "0.14.1"
libraryDependencies += "io.circe" %% "circe-parser" % "0.14.1"
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.1.0"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.10.0"
