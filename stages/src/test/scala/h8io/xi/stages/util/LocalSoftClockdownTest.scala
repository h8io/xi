package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}
import org.scalamock.handlers.CallHandler0
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
      inside(cdOnDone.onSuccess()) {
        case State.Success(LocalSoftClockdown.Tail(`ts`, `now`, 1, dispose, `nextStage`)) =>
          (onDone.dispose _).expects()
          dispose()
      }
    }
  }

  "Tail" should "return Head when the time budget is over on Stage execution" in {
    testTailForStageOverdue(0)
    testTailForStageOverdue(Random.nextLong())
    testTailForStageOverdue(Long.MinValue)
    testTailForStageOverdue(Long.MaxValue)
  }

  private def testTailForStageOverdue(startTime: Long): Unit = {
    val now = mock[() => Long]("now")
    val stage1 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 1")
    val onDone1 = mock[OnDone[ZoneOffset, OffsetDateTime, Nothing]]("OnDone 1")
    val in1 = ZoneOffset.of("+09:00")
    val out1 = OffsetDateTime.now(in1)
    (stage1.apply _).expects(in1).returns(Yield.Some(out1, onDone1))
    (now.apply _).expects().returns(startTime + 17)

    val cdOnDone1 = inside(LocalSoftClockdown.Tail(startTime, now, 42, onDone1.dispose _, stage1)(in1)) {
      case Yield.Some(`out1`, cdOnDone) =>
        (onDone1.dispose _).expects()
        cdOnDone.dispose()
        cdOnDone
    }

    val stage2 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 2")
    (onDone1.onSuccess _).expects().returns(State.Success(stage2))
    (now.apply _).expects().returns(startTime + 21)

    val cdStage2 = inside(cdOnDone1.onSuccess()) { case State.Success(cdStage) => cdStage }
    inside(cdStage2) { case LocalSoftClockdown.Tail(`startTime`, `now`, 42, dispose, `stage2`) =>
      (onDone1.dispose _).expects()
      dispose()
    }

    val onDone2 = mock[OnDone[ZoneOffset, OffsetDateTime, Nothing]]("OnDone 2")
    val in2 = ZoneOffset.UTC
    (stage2.apply _).expects(in2).returns(Yield.None(onDone2))
    (now.apply _).expects().returns(startTime + 30)

    val cdOnDone2 = inside(cdStage2(in2)) { case Yield.None(cdOnDone) => cdOnDone }

    val stage3 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 3")
    (onDone2.onSuccess _).expects().returns(State.Success(stage3))
    (now.apply _).expects().returns(startTime + 41)

    val cdStage3 = inside(cdOnDone2.onSuccess()) { case State.Success(cdStage) => cdStage }
    inside(cdStage3) { case LocalSoftClockdown.Tail(`startTime`, `now`, 42, dispose, `stage3`) =>
      (onDone2.dispose _).expects()
      dispose()
    }

    (now.apply _).expects().returns(startTime + 42)

    inside(cdStage3(ZoneOffset.of("+12:00"))) { case Yield.None(cdOnDone) =>
      cdOnDone.onSuccess() should matchPattern { case State.Complete(LocalSoftClockdown.Head(`now`, 42, `stage3`)) => }
      (onDone2.dispose _).expects()
      cdOnDone.dispose()
    }
  }

  it should "return Head when the time budget is over on OnDone execution" in {
    testTailForOnDoneOverdue(0)
    testTailForOnDoneOverdue(Random.nextLong())
    testTailForOnDoneOverdue(Long.MinValue)
    testTailForOnDoneOverdue(Long.MaxValue)
  }

  private def testTailForOnDoneOverdue(startTime: Long): Unit = {
    val now = mock[() => Long]("now")
    val stage1 = mock[Stage[ZoneId, OffsetDateTime, Nothing]]("Stage 1")
    val onDone1 = mock[OnDone[ZoneId, OffsetDateTime, Nothing]]("OnDone 1")
    val in1 = ZoneId.of("Asia/Tokyo")
    val out1 = OffsetDateTime.now(in1)
    (stage1.apply _).expects(in1).returns(Yield.Some(out1, onDone1))
    (now.apply _).expects().returns(startTime + 17)

    val cdOnDone1 = inside(LocalSoftClockdown.Tail(startTime, now, 42, onDone1.dispose _, stage1)(in1)) {
      case Yield.Some(`out1`, cdOnDone) =>
        (onDone1.dispose _).expects()
        cdOnDone.dispose()
        cdOnDone
    }

    val stage2 = mock[Stage[ZoneId, OffsetDateTime, Nothing]]("Stage 2")
    (onDone1.onSuccess _).expects().returns(State.Success(stage2))
    (now.apply _).expects().returns(startTime + 21)

    val cdStage2 = inside(cdOnDone1.onSuccess()) { case State.Success(cdStage) => cdStage }
    inside(cdStage2) { case LocalSoftClockdown.Tail(`startTime`, `now`, 42, dispose, `stage2`) =>
      (onDone1.dispose _).expects()
      dispose()
    }

    val onDone2 = mock[OnDone[ZoneId, OffsetDateTime, Nothing]]("OnDone 2")
    val in2 = ZoneId.of("Canada/Newfoundland")
    (stage2.apply _).expects(in2).returns(Yield.None(onDone2))
    (now.apply _).expects().returns(startTime + 30)

    val cdOnDone2 = inside(cdStage2(in2)) { case Yield.None(cdOnDone) =>
      (onDone2.dispose _).expects()
      cdOnDone.dispose()
      cdOnDone
    }

    val stage3 = mock[Stage[ZoneId, OffsetDateTime, Nothing]]("Stage 3")
    (onDone2.onSuccess _).expects().returns(State.Success(stage3))
    (now.apply _).expects().returns(startTime + 33)

    val cdStage3 = inside(cdOnDone2.onSuccess()) { case State.Success(cdStage) => cdStage }
    inside(cdStage3) { case LocalSoftClockdown.Tail(`startTime`, `now`, 42, dispose, `stage3`) =>
      (onDone2.dispose _).expects()
      dispose()
    }

    val onDone3 = mock[OnDone[ZoneId, OffsetDateTime, Nothing]]("OnDone 3")
    val in3 = ZoneId.of("Antarctica/South_Pole")
    val out3 = OffsetDateTime.now(in3)
    (stage3.apply _).expects(in3).returns(Yield.Some(out3, onDone3))
    (now.apply _).expects().returns(startTime + 40)

    val cdOnDone4 = inside(cdStage3(in3)) {
      case Yield.Some(`out3`, cdOnDone) =>
        (onDone3.dispose _).expects()
        cdOnDone.dispose()
        cdOnDone
    }

    val stage4 = mock[Stage[ZoneId, OffsetDateTime, Nothing]]("Stage 4")
    (onDone3.onSuccess _).expects().returns(State.Success(stage4))
    (now.apply _).expects().returns(startTime + 43)

    val cdStage4 = inside(cdOnDone4.onSuccess()) { case State.Complete(cdStage) => cdStage }
    cdStage4 shouldBe LocalSoftClockdown.Head(`now`, 42, `stage4`)

    (onDone3.dispose _).expects()
    cdOnDone4.dispose()
  }

  it should "return Head as a next stage on onComplete OnDone call" in
    testNonSuccessOnDoneCalls(onDone => (onDone.onComplete _).expects(), _.onComplete())

  it should "return Head as a next stage on onError OnDone call" in
    testNonSuccessOnDoneCalls(onDone => (onDone.onError _).expects(), _.onError())

  it should "return Head as a next stage on onPanic OnDone call" in
    testNonSuccessOnDoneCalls(onDone => (onDone.onPanic _).expects(), _.onPanic())

  private def testNonSuccessOnDoneCalls(
      callHandler: OnDone[ZoneId, ZonedDateTime, String] => CallHandler0[State[ZoneId, ZonedDateTime, String]],
      call: OnDone[ZoneId, ZonedDateTime, String] => State[ZoneId, ZonedDateTime, String]): Unit = {
    val now = mock[() => Long]("now")
    val stage = mock[Stage[ZoneId, ZonedDateTime, String]]("Stage")
    val onDone = mock[OnDone[ZoneId, ZonedDateTime, String]]("OnDone")
    val in = ZoneId.of("Asia/Tokyo")
    val out = ZonedDateTime.now(in)
    (stage.apply _).expects(in).returns(Yield.Some(out, onDone))
    (now.apply _).expects().returns(17)
    inside(LocalSoftClockdown.Tail(0, now, 42, onDone.dispose _, stage)(in)) {
      case Yield.Some(`out`, cdOnDone) =>
        val updatedStage = mock[Stage[ZoneId, ZonedDateTime, String]]("Updated stage")
        callHandler(onDone).returns(State.Success(updatedStage))
        call(cdOnDone) shouldBe State.Complete(LocalSoftClockdown.Head(now, 42, updatedStage))
        (onDone.dispose _).expects()
        cdOnDone.dispose()
        cdOnDone
    }
  }
}
