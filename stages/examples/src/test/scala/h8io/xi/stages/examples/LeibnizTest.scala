package h8io.xi.stages.examples

import h8io.xi.stages.{Signal, Yield}
import org.scalacheck.Gen
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration.DurationInt

class LeibnizTest extends AnyFlatSpec with Matchers with Inside with ScalaCheckPropertyChecks {
  "Leibniz series" should "be calculated" in {
    val stage = Leibniz.stage(100.milliseconds)
    inside(stage(())) { case Yield.Some(pi, Signal.Success, _) => pi shouldEqual (math.Pi +- 1e-3) }
  }

  "Main stage" should "return the initial stage on error" in
    forAll(Gen.zip(Gen.long, Gen.double, Gen.double)) { case (n, t, s) =>
      Leibniz.Pi(n, t, s).onError() shouldBe Leibniz.InitialStage
    }
}
