import Dependencies.*
import h8io.sbt.dependencies.*
import sbt.url

ThisBuild / organization := "io.h8"
ThisBuild / organizationName := "H8IO"
ThisBuild / organizationHomepage := Some(url("https://github.com/h8io/"))
ThisBuild / homepage := Some(url("https://github.com/h8io/xi"))

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

ThisBuild / versionScheme := Some("semver-spec")

ThisBuild / developers := List(
  Developer(
    id = "eshu",
    name = "Pavel",
    email = "tjano.xibalba@gmail.com",
    url = url("https://github.com/eshu/")))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/h8io/sbt-scoverage-summary"),
    "scm:git@github.com:h8io/sbt-scoverage-summary.git"))

ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / dynverSeparator := "-"

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / crossScalaVersions += "2.12.20"

ThisBuild / scalacOptions ++=
  Seq("-Xsource:3", "-language:higherKinds", "--deprecation", "--feature", "--unchecked", "-Xlint:_",
    "-Xfatal-warnings", "-opt:l:inline", "-opt-warnings")

ThisBuild / scalacOptions ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => Seq("--explain-types", "--language:_", "-Wunused:_", "-Wdead-code")
    case _ => Seq("-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-unused:-nowarn", "-Ypartial-unification")
  })

ThisBuild / javacOptions ++= Seq("-target", "8")

ThisBuild / libraryDependencies ++= TestBundle % Test

val `stages-core` = (project in file("stages/core")).settings(
  name := "xi-stages-core",
  libraryDependencies ++= TestBundle % TestKit,
  testkitPublishClassifier := true
).enablePlugins(TestKitPlugin)

val `stages-std` =
  (project in file("stages/std")).settings(name := "xi-stages-std", libraryDependencies ++= Cats)
    .dependsOn(`stages-core`, `stages-core` % "test->testkit")

val stages = (project in file("stages")).settings(name := "xi-stages").dependsOn(`stages-core`, `stages-std`)

val `stages-examples` = (project in file("stages/examples")).settings(
  name := "xi-stages-examples",
  publish / skip := true,
  publishLocal / skip := true,
  Compile / packageBin / mappings := Nil,
  Compile / packageDoc / mappings := Nil,
  Compile / packageSrc / mappings := Nil,
  Compile / doc / skip := true
).dependsOn(stages, `stages-core` % "test->testkit")

val cfg = (project in file("cfg"))
  .settings(
    name := "xi-cfg",
    libraryDependencies ++= Config ++ Cats ++ (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) => Seq(ScalaCollectionCompat)
      case _ => Nil
    })
  )

val lang = (project in file("lang")).settings(
  name := "xi-lang",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
).dependsOn(`stages-core`)

val root =
  (project in file(".")).settings(name := "xi")
    .aggregate(stages, `stages-core`, `stages-std`, lang, cfg, `stages-examples`)
    .enablePlugins(ScoverageSummaryPlugin)
