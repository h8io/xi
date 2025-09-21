import Dependencies.*

val scala212 = "2.12.20"
val scala213 = "2.13.16"

ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := List(scala212, scala213)

ThisBuild / organization := "io.h8"
ThisBuild / version := "0.1.0"

ThisBuild / scalacOptions ++=
  Seq("-Xsource:3", "--deprecation", "--feature", "--unchecked", "-Xlint:_", "-Xfatal-warnings")

ThisBuild / scalacOptions ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => Seq("--explain-types", "--language:_", "-Wunused:_", "-Wdead-code")
    case _ => Seq("-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-unused:-nowarn")
  })

ThisBuild / libraryDependencies ++= Cats ++ TestBundle

val stages = (project in file("stages")).settings(name := "xi-stages")

val cfg = (project in file("cfg"))
  .settings(name := "xi-cfg", libraryDependencies += "com.typesafe" % "config" % "1.4.5")

val root = (project in file(".")).settings(name := "xi").aggregate(stages, cfg).enablePlugins(ScoverageSummaryPlugin)
