import h8io.sbt.dependencies.*
import sbt.*

object Dependencies {
  private val CatsVersion = "2.13.0"

  val Cats: Seq[ModuleID] = "org.typelevel" %% Seq("cats-core") % CatsVersion

  val TestBundle: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.2.19",
    "org.scalacheck" %% "scalacheck" % "1.19.0",
    "org.scalamock" %% "scalamock" % "7.5.0",
    "org.typelevel" %% "cats-laws" % CatsVersion
  ) % Test
}
