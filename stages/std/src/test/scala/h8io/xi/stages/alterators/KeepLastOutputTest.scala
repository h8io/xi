package h8io.xi.stages.alterators

import h8io.xi.stages.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util.UUID

class KeepLastOutputTest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with MockFactory
    with ScalaCheckPropertyChecks
    with StagesCoreArbitraries {
  "Initial stage" should "should be None" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    KeepLastOutput(stage) shouldBe KeepLastOutput.None(stage)
  }

  "None" should "stay None if decorated stage returns Yield.None" in
    forAll { (in: UUID, signal: Signal[Exception]) =>
      val stage = mock[Stage[UUID, Instant, Exception]]
      val onSuccessStage = mock[Stage[UUID, Instant, Exception]]
      val onCompleteStage = mock[Stage[UUID, Instant, Exception]]
      val onErrorStage = mock[Stage[UUID, Instant, Exception]]
      val onDone = mock[OnDone[UUID, Instant, Exception]]
      (stage.apply _).expects(in).returns(Yield.None(signal, onDone))
      inside(KeepLastOutput.None(stage)(in)) { case Yield.None(`signal`, kloOnDone) =>
        (onDone.onSuccess _).expects().returns(onSuccessStage)
        kloOnDone.onSuccess() shouldBe KeepLastOutput.None(onSuccessStage)
        (onDone.onComplete _).expects().returns(onCompleteStage)
        kloOnDone.onComplete() shouldBe KeepLastOutput.None(onCompleteStage)
        (onDone.onError _).expects().returns(onErrorStage)
        kloOnDone.onError() shouldBe KeepLastOutput.None(onErrorStage)
      }
    }

  it should "become Some if decorated stage returns Yield.Some" in
    forAll { (in: Long, out: String, signal: Signal[Exception]) =>
      val stage = mock[Stage[Long, String, Exception]]
      val onSuccessStage = mock[Stage[Long, String, Exception]]
      val onCompleteStage = mock[Stage[Long, String, Exception]]
      val onErrorStage = mock[Stage[Long, String, Exception]]
      val onDone = mock[OnDone[Long, String, Exception]]
      (stage.apply _).expects(in).returns(Yield.Some(out, signal, onDone))
      inside(KeepLastOutput.None(stage)(in)) { case Yield.Some(`out`, `signal`, kloOnDone) =>
        (onDone.onSuccess _).expects().returns(onSuccessStage)
        kloOnDone.onSuccess() shouldBe KeepLastOutput.Some(out, onSuccessStage)
        (onDone.onComplete _).expects().returns(onCompleteStage)
        kloOnDone.onComplete() shouldBe KeepLastOutput.Some(out, onCompleteStage)
        (onDone.onError _).expects().returns(onErrorStage)
        kloOnDone.onError() shouldBe KeepLastOutput.Some(out, onErrorStage)
      }
    }

  "Some" should "keep the old output if decorated stage returns Yield.None" in
    forAll { (in: ZonedDateTime, out: ZoneId, signal: Signal[Exception]) =>
      val stage = mock[Stage[ZonedDateTime, ZoneId, Exception]]
      val onSuccessStage = mock[Stage[ZonedDateTime, ZoneId, Exception]]
      val onCompleteStage = mock[Stage[ZonedDateTime, ZoneId, Exception]]
      val onErrorStage = mock[Stage[ZonedDateTime, ZoneId, Exception]]
      val onDone = mock[OnDone[ZonedDateTime, ZoneId, Exception]]
      (stage.apply _).expects(in).returns(Yield.None(signal, onDone))
      inside(KeepLastOutput.Some(out, stage)(in)) { case Yield.Some(`out`, `signal`, kloOnDone) =>
        (onDone.onSuccess _).expects().returns(onSuccessStage)
        kloOnDone.onSuccess() shouldBe KeepLastOutput.Some(out, onSuccessStage)
        (onDone.onComplete _).expects().returns(onCompleteStage)
        kloOnDone.onComplete() shouldBe KeepLastOutput.Some(out, onCompleteStage)
        (onDone.onError _).expects().returns(onErrorStage)
        kloOnDone.onError() shouldBe KeepLastOutput.Some(out, onErrorStage)
      }
    }

  it should "memoize the new output if decorated stage returns Yield.Some" in
    forAll { (in: Array[Int], out: BigInt, newOut: BigInt, signal: Signal[Exception]) =>
      val stage = mock[Stage[Array[Int], BigInt, Exception]]
      val onSuccessStage = mock[Stage[Array[Int], BigInt, Exception]]
      val onCompleteStage = mock[Stage[Array[Int], BigInt, Exception]]
      val onErrorStage = mock[Stage[Array[Int], BigInt, Exception]]
      val onDone = mock[OnDone[Array[Int], BigInt, Exception]]
      (stage.apply _).expects(in).returns(Yield.Some(newOut, signal, onDone))
      inside(KeepLastOutput.Some(out, stage)(in)) { case Yield.Some(`newOut`, `signal`, kloOnDone) =>
        (onDone.onSuccess _).expects().returns(onSuccessStage)
        kloOnDone.onSuccess() shouldBe KeepLastOutput.Some(newOut, onSuccessStage)
        (onDone.onComplete _).expects().returns(onCompleteStage)
        kloOnDone.onComplete() shouldBe KeepLastOutput.Some(newOut, onCompleteStage)
        (onDone.onError _).expects().returns(onErrorStage)
        kloOnDone.onError() shouldBe KeepLastOutput.Some(newOut, onErrorStage)
      }
    }

  "dispose" should "call stage's dispose for None" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    (stage.dispose _).expects()
    noException should be thrownBy KeepLastOutput.None(stage).dispose()
  }

  it should "call stage's dispose for Some" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    (stage.dispose _).expects()
    noException should be thrownBy KeepLastOutput.Some(mock[AnyRef], stage).dispose()
  }
}
