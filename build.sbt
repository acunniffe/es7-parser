name := "es7"

organization := "com.opticdev.parsers"

version := "0.1.2"

scalaVersion := "2.12.4"

libraryDependencies += "com.opticdev" %% "parser-foundation" % "0.1.2"

libraryDependencies += "com.opticdev" %% "marvin-runtime" % "0.1.2"
libraryDependencies += "com.opticdev" %% "marvin-common" % "0.1.2"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.3"

//test suites
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
