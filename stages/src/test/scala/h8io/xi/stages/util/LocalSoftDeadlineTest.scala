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

class LocalSoftDeadlineTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "LocalSoftDeadline" should "return DeadEnd if duration is not positive" in {
    LocalSoftDeadline(0.hour, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftDeadline(-1.minute, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftDeadline(java.time.Duration.ZERO, mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
    LocalSoftDeadline(java.time.Duration.ofDays(-1), mock[Stage[Long, Instant, String]])(0) shouldBe DeadEnd.Yield
  }

  it should "return Head if duration is positive" in {
    val stage = mock[Stage[Instant, LocalDateTime, Nothing]]
    inside(LocalSoftDeadline(1.nanosecond, stage)) {
      case LocalSoftDeadline.Head(now, 1L, `stage`) => (now() - now()) should be < 0L
    }
    inside(LocalSoftDeadline(java.time.Duration.ofMillis(42), stage)) {
      case LocalSoftDeadline.Head(now, 42000000L, `stage`) => (now() - now()) should be < 0L
    }
  }

  "Head" should "return Tail" in {
    val now = mock[() => Long]
    val stage = mock[Stage[ZoneId, ZonedDateTime, Nothing]]
    val onDone = mock[OnDone[ZoneId, ZonedDateTime, Nothing]]
    val in = ZoneId.of("Asia/Kathmandu")
    val out = ZonedDateTime.now(in)
    (stage.apply _).expects(in).returns(Yield.Some(out, onDone))
    inside(LocalSoftDeadline.Head(now, 1, stage)(in)) { case Yield.Some(`out`, dlOnDone) =>
      val nextStage = mock[Stage[ZoneId, ZonedDateTime, Nothing]]
      (onDone.onSuccess _).expects().returns(State.Success(nextStage))
      val ts = Random.nextLong()
      (now.apply _).expects().returns(ts)
      inside(dlOnDone.onSuccess()) {
        case State.Success(LocalSoftDeadline.Tail(`ts`, `now`, 1, last, `nextStage`)) =>
          (onDone.dispose _).expects()
          last.onDone.dispose()
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

    val dlOnDone1 =
      inside(LocalSoftDeadline.Tail(startTime, now, 42, Yield.None(State.Success(DeadEnd).onDone), stage1)(in1)) {
        case Yield.Some(`out1`, dlOnDone) =>
          (onDone1.dispose _).expects()
          dlOnDone.dispose()
          dlOnDone
      }

    val stage2 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 2")
    (onDone1.onSuccess _).expects().returns(State.Success(stage2))
    (now.apply _).expects().returns(startTime + 21)

    val dlStage2 = inside(dlOnDone1.onSuccess()) { case State.Success(dlStage) => dlStage }
    inside(dlStage2) { case LocalSoftDeadline.Tail(`startTime`, `now`, 42, last, `stage2`) =>
      (onDone1.dispose _).expects()
      last.onDone.dispose()
    }

    val onDone2 = mock[OnDone[ZoneOffset, OffsetDateTime, Nothing]]("OnDone 2")
    val in2 = ZoneOffset.UTC
    (stage2.apply _).expects(in2).returns(Yield.None(onDone2))
    (now.apply _).expects().returns(startTime + 30)

    val dlOnDone2 = inside(dlStage2(in2)) { case Yield.None(dlOnDone) => dlOnDone }

    val stage3 = mock[Stage[ZoneOffset, OffsetDateTime, Nothing]]("Stage 3")
    (onDone2.onSuccess _).expects().returns(State.Success(stage3))
    (now.apply _).expects().returns(startTime + 41)

    val dlStage3 = inside(dlOnDone2.onSuccess()) { case State.Success(dlStage) => dlStage }
    inside(dlStage3) { case LocalSoftDeadline.Tail(`startTime`, `now`, 42, last, `stage3`) =>
      (onDone2.dispose _).expects()
      last.onDone.dispose()
    }

    (now.apply _).expects().returns(startTime + 42)

    inside(dlStage3(ZoneOffset.of("+12:00"))) { case Yield.None(dlOnDone) =>
      dlOnDone.onSuccess() should matchPattern { case State.Complete(LocalSoftDeadline.Head(`now`, 42, `stage3`)) => }
      (onDone2.dispose _).expects()
      dlOnDone.dispose()
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

    val dlOnDone1 =
      inside(LocalSoftDeadline.Tail(startTime, now, 42, Yield.None(State.Success(DeadEnd).onDone), stage1)(in1)) {
        case Yield.Some(`out1`, dlOnDone) =>
          (onDone1.dispose _).expects()
          dlOnDone.dispose()
          dlOnDone
      }

    val stage2 = mock[Stage[ZoneId, OffsetDateTime, Nothing]]("Stage 2")
    (onDone1.onSuccess _).expects().returns(State.Success(stage2))
    (now.apply _).expects().returns(startTime + 21)

    val dlStage2 = inside(dlOnDone1.onSuccess()) { case State.Success(dlStage) => dlStage }
    inside(dlStage2) { case LocalSoftDeadline.Tail(`startTime`, `now`, 42, last, `stage2`) =>
      (onDone1.dispose _).expects()
      last.onDone.dispose()
    }

    val onDone2 = mock[OnDone[ZoneId, OffsetDateTime, Nothing]]("OnDone 2")
    val in2 = ZoneId.of("Canada/Newfoundland")
    (stage2.apply _).expects(in2).returns(Yield.None(onDone2))
    (now.apply _).expects().returns(startTime + 30)

    val dlOnDone2 = inside(dlStage2(in2)) { case Yield.None(dlOnDone) =>
      (onDone2.dispose _).expects()
      dlOnDone.dispose()
      dlOnDone
    }

    val stage3 = mock[Stage[ZoneId, OffsetDateTime, Nothing]]("Stage 3")
    (onDone2.onSuccess _).expects().returns(State.Success(stage3))
    (now.apply _).expects().returns(startTime + 33)

    val dlStage3 = inside(dlOnDone2.onSuccess()) { case State.Success(dlStage) => dlStage }
    inside(dlStage3) { case LocalSoftDeadline.Tail(`startTime`, `now`, 42, last, `stage3`) =>
      (onDone2.dispose _).expects()
      last.onDone.dispose()
    }

    val onDone3 = mock[OnDone[ZoneId, OffsetDateTime, Nothing]]("OnDone 3")
    val in3 = ZoneId.of("Antarctica/South_Pole")
    val out3 = OffsetDateTime.now(in3)
    (stage3.apply _).expects(in3).returns(Yield.Some(out3, onDone3))
    (now.apply _).expects().returns(startTime + 40)

    val dlOnDone4 = inside(dlStage3(in3)) {
      case Yield.Some(`out3`, dlOnDone) =>
        (onDone3.dispose _).expects()
        dlOnDone.dispose()
        dlOnDone
    }

    val stage4 = mock[Stage[ZoneId, OffsetDateTime, Nothing]]("Stage 4")
    (onDone3.onSuccess _).expects().returns(State.Success(stage4))
    (now.apply _).expects().returns(startTime + 43)

    val dlStage4 = inside(dlOnDone4.onSuccess()) { case State.Complete(dlStage) => dlStage }
    dlStage4 shouldBe LocalSoftDeadline.Head(`now`, 42, `stage4`)

    (onDone3.dispose _).expects()
    dlOnDone4.dispose()
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
    inside(LocalSoftDeadline.Tail(0, now, 42, Yield.None(State.Success(DeadEnd).onDone), stage)(in)) {
      case Yield.Some(`out`, dlOnDone) =>
        val updatedStage = mock[Stage[ZoneId, ZonedDateTime, String]]("Updated stage")
        callHandler(onDone).returns(State.Success(updatedStage))
        call(dlOnDone) shouldBe State.Complete(LocalSoftDeadline.Head(now, 42, updatedStage))
        (onDone.dispose _).expects()
        dlOnDone.dispose()
        dlOnDone
    }
  }
}
