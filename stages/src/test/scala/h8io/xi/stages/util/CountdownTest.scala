package h8io.xi.stages.util

import h8io.xi.stages.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CountdownTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {

  "apply" should "create Impl object where i == n if n > 0" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    forAll(Gen.posNum[Long])(n => Countdown(n, stage) shouldBe Countdown.Impl(n, n, stage))
  }

  it should "create DeadEnd object if n <= 0" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    inside(Countdown(0, stage)) { case DeadEnd(dispose) =>
      (stage.dispose _).expects()
      noException should be thrownBy dispose()
    }
    forAll(Gen.negNum[Long]) { n =>
      inside(Countdown(n, stage)) { case DeadEnd(dispose) =>
        (stage.dispose _).expects()
        noException should be thrownBy dispose()
      }
    }
  }

  "Impl" should "return a yield with state Complete or Error when i == 1" in
    forAll(
      Gen.zip(Gen.posNum[Long], Arbitrary.arbitrary[Short]),
      Arbitrary.arbitrary[YieldSupplier[Short, String, Byte]]) { (parameters, yieldSupplier) =>
      val (n, in) = parameters
      val onDone = mock[OnDone[Short, String, Byte]]("onDone")
      val `yield` = yieldSupplier(onDone)
      val stage = mock[Stage[Short, String, Byte]]("underlying stage")
      val expectedState = if (`yield`.state == State.Success) State.Complete else `yield`.state
      (stage.apply _).expects(in).returns(`yield`)
      val cdOnDone = inside((`yield`, Countdown.Impl(1, n, stage)(in))) {
        case (Yield.Some(out, _, _), Yield.Some(cdOut, `expectedState`, onDone)) =>
          cdOut shouldBe out
          onDone
        case (Yield.None(_, _), Yield.None(`expectedState`, onDone)) => onDone
      }

      val onSuccessStage = mock[Stage[Short, String, Byte]]
      (onDone.onSuccess _).expects().returns(onSuccessStage)
      cdOnDone.onSuccess() shouldBe Countdown.Impl(n, n, onSuccessStage)

      val onCompleteStage = mock[Stage[Short, String, Byte]]
      (onDone.onComplete _).expects().returns(onCompleteStage)
      cdOnDone.onComplete() shouldBe Countdown.Impl(n, n, onCompleteStage)

      val onErrorStage = mock[Stage[Short, String, Byte]]
      (onDone.onError _).expects().returns(onErrorStage)
      cdOnDone.onError() shouldBe Countdown.Impl(n, n, onErrorStage)
    }

  it should "return yield with the same state if i > 1" in {
    forAll(
      Gen.zip(Gen.choose(2, 1000), Gen.choose(1L, 1000), Arbitrary.arbitrary[String]),
      Arbitrary.arbitrary[YieldSupplier[String, Long, Exception]]) { (parameters, yieldSupplier) =>
      val (i, k, in) = parameters
      val n = i + k
      test(2, n, in, yieldSupplier)
      test(i, n, in, yieldSupplier)
      test(n, n, in, yieldSupplier)
    }

    def test(i: Long, n: Long, in: String, yieldSupplier: YieldSupplier[String, Long, Exception]) = {
      val onDone = mock[OnDone[String, Long, Exception]]
      val `yield` = yieldSupplier(onDone)
      val stage = mock[Stage[String, Long, Exception]]("underlying stage")
      (stage.apply _).expects(in).returns(`yield`)
      val cdOnDone = inside((`yield`, Countdown.Impl(i, n, stage)(in))) {
        case (Yield.Some(out, _, _), Yield.Some(cdOut, `yield`.state, onDone)) =>
          cdOut shouldBe out
          onDone
        case (Yield.None(_, _), Yield.None(`yield`.state, onDone)) => onDone
      }

      val onSuccessStage = mock[Stage[String, Long, Exception]]("stage on success")
      (onDone.onSuccess _).expects().returns(onSuccessStage)
      cdOnDone.onSuccess() shouldBe Countdown.Impl(i - 1, n, onSuccessStage)

      val onCompleteStage = mock[Stage[String, Long, Exception]]("stage on complete")
      (onDone.onComplete _).expects().returns(onCompleteStage)
      cdOnDone.onComplete() shouldBe Countdown.Impl(n, n, onCompleteStage)

      val onErrorStage = mock[Stage[String, Long, Exception]]("stage on error")
      (onDone.onError _).expects().returns(onErrorStage)
      cdOnDone.onError() shouldBe Countdown.Impl(n, n, onErrorStage)
    }
  }

  "dispose" should "call stage's dispose" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    (stage.dispose _).expects()
    noException should be thrownBy Countdown.Impl(1, 42, stage).dispose()
    (stage.dispose _).expects()
    noException should be thrownBy Countdown.Impl(27, 42, stage).dispose()
    (stage.dispose _).expects()
    noException should be thrownBy Countdown.Impl(42, 42, stage).dispose()
  }
}
