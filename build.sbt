name := "macrobuf"

organization := "me.alexbool"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    "org.scala-lang"      %  "scala-reflect" % "2.10.3",
    "com.google.protobuf" %  "protobuf-java" % "2.5.0",
    "org.scalatest"       %% "scalatest"     % "2.0.M6-SNAP8" % "test"
)

scalacOptions ++= Seq("-feature", "-deprecation")

org.scalastyle.sbt.ScalastylePlugin.Settings

fork := true
