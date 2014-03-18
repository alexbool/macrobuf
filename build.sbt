name := "macrobuf"

organization := "me.alexbool"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.0-RC1"

libraryDependencies ++= Seq(
    "org.scala-lang"      %  "scala-reflect" % "2.11.0-RC1",
    "com.google.protobuf" %  "protobuf-java" % "2.5.0",
    "org.scalatest"       %% "scalatest"     % "2.1.0" % "test"
)

scalacOptions ++= Seq("-feature", "-deprecation")

org.scalastyle.sbt.ScalastylePlugin.Settings

fork := true

incOptions := incOptions.value.withNameHashing(true)

javaOptions in run += "-Xmx4G"
