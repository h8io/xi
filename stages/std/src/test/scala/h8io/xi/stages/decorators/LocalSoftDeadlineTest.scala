package h8io.xi.stages.decorators

import h8io.xi.stages.std.DeadEnd
import h8io.xi.stages.{OnDone, Stage}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Duration
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class LocalSoftDeadlineTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks {
  "LocalSoftDeadline" should "return DeadEnd if Scala duration is not positive" in
    forAll(Gen.choose(Long.MinValue, 0L)) { nanos =>
      val stage = mock[Stage[Any, Nothing, Nothing]]
      LocalSoftDeadline(FiniteDuration(nanos, TimeUnit.NANOSECONDS), stage) shouldBe DeadEnd
      LocalSoftDeadline(Duration.ofNanos(nanos), stage) shouldBe DeadEnd
    }

  it should "return Head if duration is positive" in
    forAll(Gen.choose(1L, Long.MaxValue)) { nanos =>
      val stage = mock[Stage[Any, Nothing, Nothing]]

      def test(lsdStage: Stage[Any, Nothing, Nothing]): Assertion =
        inside(lsdStage) { case LocalSoftDeadline.Head(now, `nanos`, `stage`) => now() should be < now() }

      test(LocalSoftDeadline(FiniteDuration(nanos, TimeUnit.NANOSECONDS), stage))
      test(LocalSoftDeadline(Duration.ofNanos(nanos), stage))
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
