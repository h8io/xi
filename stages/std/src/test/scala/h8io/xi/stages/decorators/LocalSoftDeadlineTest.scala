package h8io.xi.stages.decorators

import h8io.xi.stages.*
import h8io.xi.stages.std.DeadEnd
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Duration as jDuration
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

  it should "return initial stage (tsProvider == now) if duration is positive" in
    forAll(Gen.choose(1L, Long.MaxValue)) { nanos =>
      val stage = mock[Stage[Any, Nothing, Nothing]]

      def test(lsdStage: Stage[Any, Nothing, Nothing]): Assertion =
        inside(lsdStage) { case LocalSoftDeadline(tsSupplier, now, `nanos`, `stage`) =>
          tsSupplier shouldBe now
          now() should be < now()
        }

      test(LocalSoftDeadline(FiniteDuration(nanos, TimeUnit.NANOSECONDS), stage))
      test(LocalSoftDeadline(jDuration.ofNanos(nanos), stage))
    }

  "_OnDone" should "return Tail on success and Head on complete and on error" in
    forAll(Gen.zip(Gen.function0(Gen.long), Gen.posNum[Long])) { case (tsSupplier, duration) =>
      val now = mock[() => Long]("now")
      val onDone = mock[OnDone[Any, Nothing, Nothing]]("onDone")
      val _onDone = LocalSoftDeadline._OnDone(tsSupplier, now, duration, onDone)

      val onSuccessStage = mock[Stage[Any, Nothing, Nothing]]("onSuccess stage")
      (onDone.onSuccess _).expects().returns(onSuccessStage)
      _onDone.onSuccess() shouldBe LocalSoftDeadline(tsSupplier, now, duration, onSuccessStage)

      val onCompleteStage = mock[Stage[Any, Nothing, Nothing]]("onComplete stage")
      (onDone.onComplete _).expects().returns(onCompleteStage)
      _onDone.onComplete() shouldBe LocalSoftDeadline(now, now, duration, onCompleteStage)

      val onErrorStage = mock[Stage[Any, Nothing, Nothing]]("onError stage")
      (onDone.onError _).expects().returns(onErrorStage)
      _onDone.onError() shouldBe LocalSoftDeadline(now, now, duration, onErrorStage)
    }
}
