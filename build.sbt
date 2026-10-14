import Dependencies.*
import h8io.sbt.dependencies.*

val ProjectName = "xi"

organization := "io.h8"
organizationName := "H8IO"
organizationHomepage := Some(uri("https://github.com/h8io/"))
homepage := Some(uri(s"https://github.com/h8io/$ProjectName"))
scmInfo := Some(ScmInfo(uri(s"https://github.com/h8io/$ProjectName"), s"scm:git@github.com:h8io/$ProjectName.git"))
licenses := List(License.Apache2)
developers := List(
  Developer(id = "eshu", name = "Pavel", email = "tjano.xibalba@gmail.com", url = uri("https://github.com/eshu/")))

scalaVersion := "2.13.18"
crossScalaVersions += "2.12.21"
javacOptions ++= Seq("-target", "8")

versionScheme := Some("semver-spec")
// sbt-dynver computes the version once for the whole build, from these two in ThisBuild
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / dynverSeparator := "-"

scalacOptions ++=
  Seq("-Xsource:3", "-language:higherKinds", "--deprecation", "--feature", "--unchecked", "-Xlint:_",
    "-Xfatal-warnings", "-opt:l:inline", "-opt-warnings")

scalacOptions ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) => Seq("--explain-types", "--language:_", "-Wunused:_", "-Wdead-code")
    case Some((2, 12)) => Seq("-Ywarn-unused", "-Ywarn-dead-code", "-Ywarn-unused:-nowarn", "-Ypartial-unification")
    case _ => Nil
  })

coverageSummaryStmtLowThreshold := 100
coverageSummaryStmtHighThreshold := 100
coverageSummaryBranchLowThreshold := 100
coverageSummaryBranchHighThreshold := 100

libraryDependencies ++= TestBundle % Test

val root = (project in file(".")).settings(
  name := ProjectName,
  libraryDependencies ++= H8IO
).enablePlugins(ScoverageSummaryPlugin)

val pages = (project in file("pages"))
  .settings(
    name := "xi-pages",
    publish / skip := true,
    publishLocal / skip := true,
    // sbt 2 moves every target under the root's target/out; npm and release.sh expect the site's inputs here
    cleanFiles += baseDirectory.value / "target",
    ScalaUnidoc / unidoc / target := baseDirectory.value / "target" / s"scala-${scalaBinaryVersion.value}" / "unidoc",
    mdocIn := (ThisBuild / baseDirectory).value / "docs",
    mdocOut := baseDirectory.value / "target" / "mdoc",
    mdocVariables :=
      Map("VERSION" -> (if (isSnapshot.value) previousStableVersion.value else None).getOrElse(version.value))
  )
  .dependsOn(root)
  .enablePlugins(ScalaUnidocPlugin, MdocPlugin)
