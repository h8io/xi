import Dependencies.*
import h8io.sbt.dependencies.*

val ProjectName = "xi"

val ScalaVersion213 = "2.13.18"

// Every branch on the Scala version lists the versions it knows and ends here, so that a new one fails the build until
// each of them has been looked at
def unsupportedScala(version: String): Nothing = throw new MessageOnlyException(s"Unsupported Scala version $version")

organization := "io.h8"
organizationName := "H8IO"
organizationHomepage := Some(uri("https://github.com/h8io/"))
homepage := Some(uri(s"https://github.com/h8io/$ProjectName"))
scmInfo := Some(ScmInfo(uri(s"https://github.com/h8io/$ProjectName"), s"scm:git@github.com:h8io/$ProjectName.git"))
licenses := List(License.Apache2)
developers := List(
  Developer(id = "eshu", name = "Pavel", email = "tjano.xibalba@gmail.com", url = uri("https://github.com/eshu/")))

scalaVersion := ScalaVersion213
crossScalaVersions += "3.9.0"
javacOptions ++= Seq("--release", "11")

versionScheme := Some("semver-spec")
// sbt-dynver computes the version once for the whole build, from these two in ThisBuild
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / dynverSeparator := "-"

scalacOptions ++=
  (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) =>
      Seq(
        "-Xsource:3",
        "--deprecation",
        "--feature",
        "--unchecked",
        "-Xlint:_",
        "-Xfatal-warnings",
        "-opt:l:inline",
        "-opt-warnings",
        "--explain-types",
        "--language:_",
        "-Wunused:_",
        "-Wdead-code",
        "-Wvalue-discard",
        "-Wnonunit-statement"
      )
    case Some((3, 9)) =>
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Wall",
        "-Werror",
        "-explain",
        "-opt",
        "-Wopt:at-inline-failed-summary",
        // Scala 2 needs the trailing `_` to eta-expand a method without an expected function type, Scala 3 does not
        "-Wconf:msg=eta-expansion is unnecessary:s"
      )
    case _ => unsupportedScala(scalaVersion.value)
  })

// For code whose statements are values on purpose: a ScalaMock expectation or a ScalaTest assertion in tests, an
// expression shown with its result in the docs. Those are spared the warnings about discarded values. On 2.13 the
// category of -Wnonunit-statement is shared with the warning about a pure expression in statement position, which they
// lose as well.
def lenientScalacOptions(scalaVersion: String): Seq[String] =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => Seq("-Wconf:cat=w-flag-value-discard:s,cat=other-pure-statement:s")
    case Some((3, 9)) => Seq("-Wconf:name=ValueDiscarding:s,name=UnusedNonUnitValue:s")
    case _ => unsupportedScala(scalaVersion)
  }

Test / scalacOptions ++= lenientScalacOptions(scalaVersion.value)

coverageSummaryStmtLowThreshold := 100
coverageSummaryStmtHighThreshold := 100
coverageSummaryBranchLowThreshold := 100
coverageSummaryBranchHighThreshold := 100

libraryDependencies ++= TestBundle % Test

val core = (project in file("core")).settings(
  name := "xi-core",
  libraryDependencies ++= H8IO
)

// Spark publishes no Scala 3 build. spark still lists 3.9 in its crossScalaVersions, because `+` runs a task on the
// root alone wherever the root supports the version, and the root's aggregation would then pull a 2.13-only spark onto
// a core switched to 3.9. On 3.9 spark is empty instead: no sources, no Spark, nothing published.
def sparkSupported(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, 13)) => true
    case Some((3, 9)) => false
    case _ => unsupportedScala(scalaVersion)
  }

val spark = (project in file("spark"))
  .settings(
    name := "xi-spark",
    libraryDependencies ++= (if (sparkSupported(scalaVersion.value)) Spark % Provided else Nil),
    Compile / unmanagedSourceDirectories :=
      (if (sparkSupported(scalaVersion.value)) (Compile / unmanagedSourceDirectories).value else Nil),
    Test / unmanagedSourceDirectories :=
      (if (sparkSupported(scalaVersion.value)) (Test / unmanagedSourceDirectories).value else Nil),
    publish / skip := !sparkSupported(scalaVersion.value),
    publishLocal / skip := !sparkSupported(scalaVersion.value)
  )
  .dependsOn(core)

val root = (project in file("."))
  .enablePlugins(ScoverageSummaryPlugin)
  .settings(name := ProjectName, publish / skip := true, publishLocal / skip := true)
  .aggregate(core, spark)

val pages = (project in file("pages"))
  .settings(
    name := "xi-pages",
    publish / skip := true,
    publishLocal / skip := true,
    scalacOptions ++= lenientScalacOptions(scalaVersion.value),
    // sbt 2 moves every target under the root's target/out; npm and release.sh expect the site's inputs here
    cleanFiles += baseDirectory.value / "target",
    ScalaUnidoc / unidoc / target := baseDirectory.value / "target" / s"scala-${scalaBinaryVersion.value}" / "unidoc",
    mdocIn := (ThisBuild / baseDirectory).value / "docs",
    mdocOut := baseDirectory.value / "target" / "mdoc",
    mdocVariables :=
      Map("VERSION" -> (if (isSnapshot.value) previousStableVersion.value else None).getOrElse(version.value))
  )
  .dependsOn(core, spark)
  .enablePlugins(ScalaUnidocPlugin, MdocPlugin)
