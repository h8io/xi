package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Signal, Stage, Yield}
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Duration
import java.util.UUID
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class LocalSoftDeadlineTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks {
  "LocalSoftDeadline" should "return DeadEnd if Scala duration is not positive" in
    forAll(Gen.choose(Long.MinValue, 0L)) { nanos =>
      LocalSoftDeadline(FiniteDuration(nanos, TimeUnit.NANOSECONDS)) shouldBe DeadEnd
      LocalSoftDeadline(Duration.ofNanos(nanos)) shouldBe DeadEnd
    }

  it should "return Head if duration is positive" in
    forAll(Gen.choose(1L, Long.MaxValue)) { nanos =>
      def test(stage: Stage.Endo[?, Nothing]): Assertion =
        inside(stage) { case LocalSoftDeadline.Head(now, `nanos`) => now() should be < now() }

      test(LocalSoftDeadline(FiniteDuration(nanos, TimeUnit.NANOSECONDS)))
      test(LocalSoftDeadline(Duration.ofNanos(nanos)))
    }

  "Head" should "return the correct yield" in
    forAll(Gen.zip(Gen.long, Gen.choose(1L, Long.MaxValue), Arbitrary.arbitrary[String])) { case (ts, duration, in) =>
      val now = mock[() => Long]
      val head = LocalSoftDeadline.Head[String](now, duration)
      val tail = LocalSoftDeadline.Tail(ts, head)
      (now.apply _).expects().returns(ts)
      head(in) shouldBe Yield.Some(in, Signal.Success, OnDone.FromStage(tail))
    }

  "Tail" should "return Yield.Some with Success signal if not overdue" in
    forAll(Gen.zip(Gen.long, Gen.choose(1L, Long.MaxValue), Gen.uuid)) { case (ts, duration, in) =>
      def test(passed: Long): Assertion = {
        val now = mock[() => Long]
        val head = LocalSoftDeadline.Head[UUID](now, duration)
        val tail = LocalSoftDeadline.Tail(ts, head)
        (now.apply _).expects().returns(ts + passed)
        inside(tail(in)) { case Yield.Some(`in`, Signal.Success, onDone) =>
          onDone.onSuccess() shouldBe tail
          onDone.onComplete() shouldBe head
          onDone.onError() shouldBe head
        }
      }
      forAll(Gen.choose(1, duration)) { passed =>
        test(0)
        test(passed)
      }
    }

  it should "return Yield.Some with Complete signal if overdue" in
    forAll(Gen.zip(Gen.long, Gen.choose(1L, Long.MaxValue - 1), Gen.uuid)) { case (ts, duration, in) =>
      def test(passed: Long): Assertion = {
        val now = mock[() => Long]
        val head = LocalSoftDeadline.Head[UUID](now, duration)
        val tail = LocalSoftDeadline.Tail(ts, head)
        (now.apply _).expects().returns(ts + passed)
        tail(in) shouldBe Yield.Some(in, Signal.Complete, OnDone.FromStage(head))
      }
      forAll(Gen.choose(duration + 1, Long.MaxValue)) { passed =>
        test(duration)
        test(passed)
      }
    }
}
