package h8io.xi.stages.util

import h8io.xi.stages.*
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID

class CacheTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {
  "Cache" should "cache output only if the yield is Some and the state is Success" in
    forAll(Gen.zip(genOnDoneToYield[UUID, String, Exception].arbitrary, Gen.uuid)) { case (yieldSupplier, in) =>
      val stage = mock[Stage[UUID, String, Exception]]("underlying stage")
      val onDone = mock[OnDone[UUID, String, Exception]]("underlying onDone")
      val `yield` = yieldSupplier(onDone)
      val cache = Cache(stage)
      (stage.apply _).expects(in).returns(`yield`)
      val cacheYield = cache(in)
      inside((`yield`, cacheYield)) {
        case (Yield.Some(out, state, _), Yield.Some(cacheOut, cacheState, cacheOnDone)) =>
          cacheOut shouldBe out
          cacheState shouldBe state
          val onSuccessStage = mock[Stage[UUID, String, Exception]]("on success stage (Yield.Some)")
          (onDone.onSuccess _).expects().returns(onSuccessStage)
          if (state == Signal.Success) cacheOnDone.onSuccess() shouldBe Cache.Cached(out, onSuccessStage)
          else cacheOnDone.onSuccess() shouldBe Cache(onSuccessStage)
        case (Yield.None(state, _), Yield.None(cacheState, cacheOnDone)) =>
          cacheState shouldBe state
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

  "Cached" should "keep output while the state is Success" in
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
