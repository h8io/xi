package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.*
import scala.concurrent.duration.DurationInt
import scala.util.Random

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

  "Head" should "return Tail" in {
    val now = mock[() => Long]
    val stage = mock[Stage[ZoneId, ZonedDateTime, Nothing]]
    val onDone = mock[OnDone[ZoneId, ZonedDateTime, Nothing]]
    val in = ZoneId.of("Asia/Kathmandu")
    val out = ZonedDateTime.now(in)
    (stage.apply _).expects(in).returns(Yield.Some(out, onDone))
    inside(LocalSoftClockdown.Head(now, 1, stage)(in)) { case Yield.Some(`out`, cdOnDone) =>
      val nextStage = mock[Stage[ZoneId, ZonedDateTime, Nothing]]
      (onDone.onSuccess _).expects().returns(State.Success(nextStage))
      val ts = Random.nextLong()
      (now.apply _).expects().returns(ts)
      cdOnDone.onSuccess() should matchPattern {
        case State.Success(LocalSoftClockdown.Tail(`ts`, `now`, 1, `nextStage`)) =>
      }
    }
  }

  "Tail" should "return Head when the time budget is over" in {
    testTail(0)
//    testTail(Random.nextLong())
//    testTail(Long.MinValue)
//    testTail(Long.MaxValue)
  }

  private def testTail(startTime: Long): Unit = {
    val now = mock[() => Long]("now")
    val stage1 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 1")
    val onDone1 = mock[OnDone[ZoneOffset, OffsetDateTime, Nothing]]("OnDone 1")
    val in1 = ZoneOffset.of("+09:00")
    val out1 = OffsetDateTime.now(in1)
    (stage1.apply _).expects(in1).returns(Yield.Some(out1, onDone1))
    (now.apply _).expects().returns(startTime + 17)

    val cdOnDone1 = inside(LocalSoftClockdown.Tail(startTime, now, 42, stage1)(in1)) {
      case Yield.Some(`out1`, cdOnDone) => cdOnDone
    }

    val stage2 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 2")
    (onDone1.onSuccess _).expects().returns(State.Success(stage2))

    val cdStage2 = inside(cdOnDone1.onSuccess()) { case State.Success(cdStage) =>
      cdStage shouldBe a[LocalSoftClockdown.Tail[?, ?, ?]]
      cdStage
    }

    val onDone2 = mock[OnDone[ZoneOffset, OffsetDateTime, Nothing]]("OnDone 2")
    val in2 = ZoneOffset.UTC
    (stage2.apply _).expects(in2).returns(Yield.None(onDone2))
    (now.apply _).expects().returns(startTime + 30)

    val cdOnDone2 = inside(cdStage2(in2)) { case Yield.None(cdOnDone) => cdOnDone }

    val stage3 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 3")
    (onDone2.onSuccess _).expects().returns(State.Success(stage3))

    val cdStage3 = inside(cdOnDone2.onSuccess()) { case State.Success(cdStage) =>
      cdStage shouldBe a[LocalSoftClockdown.Tail[?, ?, ?]]
      cdStage
    }

    (now.apply _).expects().returns(startTime + 42)

    inside(cdStage3(ZoneOffset.of("+12:00"))) { case Yield.None(cdOnDone) =>
      cdOnDone.onSuccess() should matchPattern { case State.Complete(LocalSoftClockdown.Head(`now`, 42, `stage3`)) => }
    }
  }
}
