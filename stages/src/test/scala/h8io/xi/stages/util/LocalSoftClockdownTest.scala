package h8io.xi.stages.util

import h8io.xi.stages.Stage
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, LocalDateTime}
import scala.concurrent.duration.DurationInt

class LocalSoftClockdownTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "LocalSoftClockdown" should "return DeadEnd if duration is not positive" in {
    LocalSoftClockdown(0.hour, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftClockdown(-1.minute, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftClockdown(java.time.Duration.ZERO, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftClockdown(java.time.Duration.ofDays(-1), mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
  }

  it should "return Head if duration is positive" in {
    val stage = mock[Stage[Instant, LocalDateTime, Nothing]]
    inside(LocalSoftClockdown(1.nanosecond, stage)) {
      case LocalSoftClockdown.Head(now, 1L, `stage`) => (now() - now()) should be < 0L
    }
    inside(LocalSoftClockdown(java.time.Duration.ofMillis(42), stage)) {
      case LocalSoftClockdown.Head(now, 42000000L, `stage`) => (now() - now()) should be < 0L
    }
  }
}
