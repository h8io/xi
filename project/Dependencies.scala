import sbt.*

object Dependencies {
  private val CatsVersion = "2.13.0"

  val H8IO: Seq[ModuleID] = Seq(
    "io.h8" %% "stages" % "0.1.0",
    "io.h8" %% "cfg" % "0.4.1",
    "io.h8" %% "reflect" % "0.0.6"
  )

  val TestBundle: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.2.20",
    "org.scalamock" %% "scalamock-scalatest" % "7.6.3",
    "org.typelevel" %% "cats-laws" % CatsVersion,
    "org.typelevel" %% "discipline-scalatest" % "2.3.0",
    "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0",
    "org.scalacheck" %% "scalacheck" % "1.20.0"
  )
}
