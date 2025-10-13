package h8io.xi.stages.examples

import h8io.xi.stages.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID

class CacheTest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with MockFactory
    with ScalaCheckPropertyChecks
    with CoreStagesArbitraries {
  "Cache" should "cache output only if the yield is Some and the signal is Success" in {
    def test(
        yieldSupplier: SignalAndOnDoneToYield[UUID, String, Exception],
        signal: Signal[Exception],
        in: UUID): Assertion = {
      val stage = mock[Stage[UUID, String, Exception]]("underlying stage")
      val onDone = mock[OnDone[UUID, String, Exception]]("underlying onDone")
      val `yield` = yieldSupplier(signal, onDone)
      val cache = Cache(stage)
      (stage.apply _).expects(in).returns(`yield`)
      val cacheYield = cache(in)
      inside((`yield`, cacheYield)) {
        case (Yield.Some(out, signal, _), Yield.Some(cacheOut, cacheSignal, cacheOnDone)) =>
          cacheOut shouldBe out
          cacheSignal shouldBe signal
          val onSuccessStage = mock[Stage[UUID, String, Exception]]("on success stage (Yield.Some)")
          (onDone.onSuccess _).expects().returns(onSuccessStage)
          if (signal == Signal.Success) cacheOnDone.onSuccess() shouldBe Cache.Cached(out, onSuccessStage)
          else cacheOnDone.onSuccess() shouldBe Cache(onSuccessStage)
        case (Yield.None(signal, _), Yield.None(cacheSignal, cacheOnDone)) =>
          cacheSignal shouldBe signal
          val onSuccessStage = mock[Stage[UUID, String, Exception]]("on success stage (Yield.None)")
          (onDone.onSuccess _).expects().returns(onSuccessStage)
          cacheOnDone.onSuccess() shouldBe Cache(onSuccessStage)
      }

      val onCompleteStage = mock[Stage[UUID, String, Exception]]("on complete stage")
      (onDone.onComplete _).expects().returns(onCompleteStage)
      cacheYield.onDone.onComplete() shouldBe Cache(onCompleteStage)

      val onErrorStage = mock[Stage[UUID, String, Exception]]("on error stage")
      (onDone.onError _).expects().returns(onErrorStage)
      cacheYield.onDone.onError() shouldBe Cache(onErrorStage)
    }
    forAll(
      Gen.zip(
        Arbitrary.arbitrary[SignalAndOnDoneToYield[UUID, String, Exception]],
        Arbitrary.arbitrary[Signal.Error[Exception]],
        Gen.uuid)) { case (yieldSupplier, error, in) =>
      test(yieldSupplier, Signal.Success, in)
      test(yieldSupplier, Signal.Complete, in)
      test(yieldSupplier, error, in)
    }
  }

  "Cached" should "keep output while the signal is Success" in
    forAll(Gen.zip(Gen.long, Gen.uuid)) { case (in, out) =>
      val stage = mock[Stage[Long, UUID, Exception]]("underlying stage")
      val cached = Cache.Cached(out, stage)
      inside(cached(in)) { case Yield.Some(`out`, Signal.Success, onDone) =>
        onDone.onSuccess() shouldBe cached
        onDone.onComplete() shouldBe Cache(stage)
        onDone.onError() shouldBe Cache(stage)
      }
    }
}
