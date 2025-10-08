package h8io.xi.stages.util

import h8io.xi.stages.State.Complete
import h8io.xi.stages.Yield
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DeadEndTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {
  "DeadEnd" should "return None for any argument" in {
    val dispose = mock[() => Unit]
    val stage = DeadEnd(dispose)
    stage.Yield shouldBe Yield.None(Complete, stage)
    stage(mock[AnyRef]) shouldBe stage.Yield
    (dispose.apply _).expects()
    stage.dispose()
  }
}
