package h8io.xi.stages.decorators

import h8io.xi.stages.*
import h8io.xi.stages.std.DeadEnd
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.sql.Timestamp
import java.time.{Duration as jDuration, Instant}
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class LocalSoftDeadlineTest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with MockFactory
    with ScalaCheckPropertyChecks
    with StagesArbitraries {
  "LocalSoftDeadline" should "return DeadEnd if Scala duration is not positive" in
    forAll(Gen.choose(Long.MinValue, 0L)) { nanos =>
      val stage = mock[Stage[Any, Nothing, Nothing]]
      LocalSoftDeadline(FiniteDuration(nanos, TimeUnit.NANOSECONDS), stage) shouldBe DeadEnd
      LocalSoftDeadline(jDuration.ofNanos(nanos), stage) shouldBe DeadEnd
    }

  it should "return Head if duration is positive" in
    forAll(Gen.choose(1L, Long.MaxValue)) { nanos =>
      val stage = mock[Stage[Any, Nothing, Nothing]]

      def test(lsdStage: Stage[Any, Nothing, Nothing]): Assertion =
        inside(lsdStage) { case LocalSoftDeadline.Head(now, `nanos`, `stage`) => now() should be < now() }

      test(LocalSoftDeadline(FiniteDuration(nanos, TimeUnit.NANOSECONDS), stage))
      test(LocalSoftDeadline(jDuration.ofNanos(nanos), stage))
    }

  "Head" should "return the same yield as the underlying stage mapped to _OnDone" in
    forAll(
      Gen.zip(
        Gen.long,
        Gen.choose(1L, Long.MaxValue),
        Arbitrary.arbitrary[String],
        arbOnDoneToYield[String, UUID, Exception].arbitrary)) {
      case (ts, duration, in, yieldSupplier) =>
        val now = mock[() => Long]("now")
        val stage = mock[Stage[String, UUID, Exception]]("underlying stage")
        val `yield` = yieldSupplier(mock[OnDone[String, UUID, Exception]])
        val head = LocalSoftDeadline.Head(now, duration, stage)
        inSequence {
          (now.apply _).expects().returns(ts)
          (stage.apply _).expects(in).returns(`yield`)
        }
        head(in) shouldBe `yield`.mapOnDone(LocalSoftDeadline._OnDone(ts, now, duration, _))
    }

  "Tail" should "return yield None with OnDone that points to Head on overdue" in
    forAll(Gen.zip(Gen.long, Gen.choose(1L, Int.MaxValue), Gen.choose(1L, Int.MaxValue), Gen.uuid)) {
      case (ts, duration, overdue, in) =>
        val now = mock[() => Long]("now")
        val stage = mock[Stage[UUID, Timestamp, Exception]]("underlying stage")
        val head = LocalSoftDeadline.Tail(ts, now, duration, stage)
        (now.apply _).expects().returns(ts + duration)
        val expectedYield = Yield.None(Signal.Complete, OnDone.FromStage(LocalSoftDeadline.Head(now, duration, stage)))
        head(in) shouldBe expectedYield
        (now.apply _).expects().returns(ts + duration + overdue)
        head(in) shouldBe expectedYield
    }

  it should "return the same yield as the underlying stage mapped to _OnDone while not overdue" in
    forAll(
      Gen.zip(
        Gen.long,
        Gen.choose(0L, Int.MaxValue),
        Gen.choose(1L, Int.MaxValue),
        Gen.finiteDuration,
        arbOnDoneToYield[FiniteDuration, Instant, String].arbitrary)) { case (ts, spent, rest, in, yieldSupplier) =>
      val duration = spent + rest
      val now = mock[() => Long]("now")
      val stage = mock[Stage[FiniteDuration, Instant, String]]("underlying stage")
      val onDone = mock[OnDone[FiniteDuration, Instant, String]]("onDone")
      val `yield` = yieldSupplier(onDone)
      val head = LocalSoftDeadline.Tail(ts, now, duration, stage)
      inSequence {
        (now.apply _).expects().returns(ts + spent)
        (stage.apply _).expects(in).returns(`yield`)
      }
      head(in) shouldBe `yield`.mapOnDone(LocalSoftDeadline._OnDone(ts, now, duration, _))
    }

  "_OnDone" should "return Tail on success and Head on complete and on error" in
    forAll(Gen.zip(Gen.long, Gen.posNum[Long])) { case (ts, duration) =>
      val now = mock[() => Long]("now")
      val onDone = mock[OnDone[Any, Nothing, Nothing]]("onDone")
      val _onDone = LocalSoftDeadline._OnDone(ts, now, duration, onDone)

      val onSuccessStage = mock[Stage[Any, Nothing, Nothing]]("onSuccess stage")
      (onDone.onSuccess _).expects().returns(onSuccessStage)
      _onDone.onSuccess() shouldBe LocalSoftDeadline.Tail(ts, now, duration, onSuccessStage)

      val onCompleteStage = mock[Stage[Any, Nothing, Nothing]]("onComplete stage")
      (onDone.onComplete _).expects().returns(onCompleteStage)
      _onDone.onComplete() shouldBe LocalSoftDeadline.Head(now, duration, onCompleteStage)

      val onErrorStage = mock[Stage[Any, Nothing, Nothing]]("onError stage")
      (onDone.onError _).expects().returns(onErrorStage)
      _onDone.onError() shouldBe LocalSoftDeadline.Head(now, duration, onErrorStage)
    }
}
