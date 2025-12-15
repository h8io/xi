import sbt.*

object Dependencies {
  private val CatsVersion = "2.13.0"

  val H8IO: Seq[ModuleID] =
    Seq("io.h8" %% "stages" % "0.0.3", "io.h8" %% "cfg" % "0.0.2", "io.h8" %% "reflect" % "0.0.4")

  val TestBundle: Seq[ModuleID] =
    Seq(
      "org.scalatest" %% "scalatest" % "3.2.19",
      "org.scalamock" %% "scalamock" % "7.5.2",
      "org.typelevel" %% "cats-laws" % CatsVersion,
      "org.typelevel" %% "discipline-scalatest" % "2.3.0",
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0",
      "org.scalacheck" %% "scalacheck" % "1.19.0"
    )
}
