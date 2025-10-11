import h8io.sbt.dependencies.*
import sbt.*

object Dependencies {
  private val CatsVersion = "2.13.0"

  val Cats: Seq[ModuleID] = "org.typelevel" %% Seq("cats-core") % CatsVersion

  val ScalaCheck: Seq[ModuleID] =
    Seq("org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0", "org.scalacheck" %% "scalacheck" % "1.19.0")

  val CatsTest: Seq[ModuleID] =
    Seq("org.typelevel" %% "cats-laws" % CatsVersion, "org.typelevel" %% "discipline-scalatest" % "2.3.0")

  val TestBundle: Seq[ModuleID] =
    (Seq(
      "org.scalatest" %% "scalatest" % "3.2.19",
      "org.scalamock" %% "scalamock" % "7.5.0"
    ) ++ CatsTest ++ ScalaCheck) % Test
}
