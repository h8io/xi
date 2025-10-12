package h8io.xi.stages.examples

import h8io.xi.stages.{Signal, Yield}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.DurationInt

class LeibnizTest extends AnyFlatSpec with Matchers with Inside {
  "Leibniz series" should "be calculated" in {
    val stage = Leibniz.stage(100.milliseconds)
    inside(stage(())) { case Yield.Some(pi, Signal.Success, onDone) =>
      pi shouldEqual (math.Pi +- 1e-3)
      onDone.onError() shouldBe stage
    }
  }
}
