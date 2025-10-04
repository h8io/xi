package h8io.xi.stages.util

import h8io.xi.stages.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CountdownTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {

  "apply" should "create Impl object where i == n if n > 0" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    Countdown(42, stage) shouldBe Countdown.Impl(42, 42, stage)
  }

  it should "create DeadEnd object if n <= 0" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    inside(Countdown(0, stage)) { case DeadEnd(dispose) =>
      (stage.dispose _).expects()
      noException should be thrownBy dispose()
    }
    inside(Countdown(Long.MinValue, stage)) { case DeadEnd(dispose) =>
      (stage.dispose _).expects()
      noException should be thrownBy dispose()
    }
  }

  private implicit def genYield[I, O: Arbitrary, E: Arbitrary]: Arbitrary[Yield[I, O, E]] =
    Arbitrary(
      for {
        isSome <- Arbitrary.arbitrary[Boolean]
        state <- Arbitrary.arbitrary[State[E]]
        onDone = mock[OnDone[I, O, E]]
        yieldGen =
          if (isSome) Arbitrary.arbitrary[O].map(Yield.Some(_, state, onDone)) else Gen.const(Yield.None(state, onDone))
        yld <- yieldGen
      } yield yld)

  "Impl" should "return a yield with state Complete or Error when i == 1" in
    forAll { (yld: Yield[Short, String, Byte]) =>
      val stage = mock[Stage[Short, String, Byte]]
      val expectedState = if (yld.state == State.Success) State.Complete else yld.state
      (stage.apply _).expects(17: Short).returns(yld)
      val cdOnDone = inside((yld, Countdown.Impl(1, 42, stage)(17))) {
        case (Yield.Some(out, _, _), Yield.Some(cdOut, `expectedState`, onDone)) =>
          cdOut shouldBe out
          onDone
        case (Yield.None(_, _), Yield.None(`expectedState`, onDone)) => onDone
      }

      val onSuccessStage = mock[Stage[Short, String, Byte]]
      (yld.onDone.onSuccess _).expects().returns(onSuccessStage)
      cdOnDone.onSuccess() shouldBe Countdown.Impl(42, 42, onSuccessStage)

      val onCompleteStage = mock[Stage[Short, String, Byte]]
      (yld.onDone.onComplete _).expects().returns(onCompleteStage)
      cdOnDone.onComplete() shouldBe Countdown.Impl(42, 42, onCompleteStage)

      val onErrorStage = mock[Stage[Short, String, Byte]]
      (yld.onDone.onError _).expects().returns(onErrorStage)
      cdOnDone.onError() shouldBe Countdown.Impl(42, 42, onErrorStage)
    }

  it should "return yield with the same state if i > 1" in {
    test(2)
    test(21)
    test(42)
    def test(i: Long): Assertion =
      forAll { (yld: Yield[String, Long, Exception]) =>
        val stage = mock[Stage[String, Long, Exception]]("underlying stage")
        (stage.apply _).expects("xi").returns(yld)
        val cdOnDone = inside((yld, Countdown.Impl(i, 42, stage)("xi"))) {
          case (Yield.Some(out, _, _), Yield.Some(cdOut, yld.state, onDone)) =>
            cdOut shouldBe out
            onDone
          case (Yield.None(_, _), Yield.None(yld.state, onDone)) => onDone
        }

        val onSuccessStage = mock[Stage[String, Long, Exception]]("stage on success")
        (yld.onDone.onSuccess _).expects().returns(onSuccessStage)
        cdOnDone.onSuccess() shouldBe Countdown.Impl(i - 1, 42, onSuccessStage)

        val onCompleteStage = mock[Stage[String, Long, Exception]]("stage on complete")
        (yld.onDone.onComplete _).expects().returns(onCompleteStage)
        cdOnDone.onComplete() shouldBe Countdown.Impl(42, 42, onCompleteStage)

        val onErrorStage = mock[Stage[String, Long, Exception]]("stage on error")
        (yld.onDone.onError _).expects().returns(onErrorStage)
        cdOnDone.onError() shouldBe Countdown.Impl(42, 42, onErrorStage)
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
