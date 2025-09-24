package h8io.xi.stages.util

import h8io.xi.stages.Stage
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.duration.DurationInt

class LocalSoftClockdownTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "LocalSoftClockdown" should "return DeadEnd if duration is not positive" in {
    LocalSoftClockdown(0.hour, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftClockdown(-1.minute, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftClockdown(java.time.Duration.ZERO, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftClockdown(java.time.Duration.ofDays(-1), mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
  }
}
