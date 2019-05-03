name          := "joiner"
organization  := "com.miodx.clonomap"
version       := "0.1.4"
description   := "joiner project"
scalaVersion  := "2.11.11"

bucketSuffix  := "era7.com"

libraryDependencies ++= Seq(
  "com.miodx.common" %% "fastarious" % "0.11.0",
  "org.typelevel"    %% "spire"      % "0.16.1",
  "org.scalatest"    %% "scalatest"  % "3.0.5"
)
// shows time for each test:
testOptions in Test += Tests.Argument("-oD")
// disables parallel exec
parallelExecution in Test := false


// NOTE should be reestablished
wartremoverErrors in (Test, compile) := Seq()
wartremoverErrors in (Compile, compile) := Seq()


// // For resolving dependency versions conflicts:
dependencyOverrides ++= Seq(
  "org.typelevel" %% "machinist" % "0.6.1"
)

// // If you need to deploy this project as a Statika bundle:
// generateStatikaMetadataIn(Compile)

// // This includes tests sources in the assembled fat-jar:
// fullClasspath in assembly := (fullClasspath in Test).value

// // This turns on fat-jar publishing during release process:
// publishFatArtifact in Release := true

// // Only for Java projects
// enablePlugin(JavaOnlySettings)
// javaVersion := "1.8"
