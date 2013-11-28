name := "macrobuf"

organization := "me.alexbool"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.0-M7"

libraryDependencies ++= Seq(
    "org.scala-lang"      %  "scala-reflect" % "2.11.0-M7",
    "com.google.protobuf" %  "protobuf-java" % "2.5.0",
    "org.scalatest"       %% "scalatest"     % "2.0.1-SNAP4" % "test"
)

scalacOptions ++= Seq("-feature", "-deprecation")

org.scalastyle.sbt.ScalastylePlugin.Settings

fork := true
