package h8io.xi.stages.std

import h8io.xi.stages.{OnDone, Stage, State, Yield}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountdownTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Countdown" should "return the yield None with the state Complete if the counter is zero" in {
    val stage = mock[Stage[String, Long, Nothing]]
    inside(Countdown.Impl(0, 3, stage).apply("xi")) { case Yield.None(onDone) =>
      val expectedState = State.Complete(Countdown.Impl(3, 3, stage))
      onDone.onSuccess() shouldBe expectedState
      onDone.onComplete() shouldBe expectedState
      onDone.onError() shouldBe expectedState
      onDone.onPanic() shouldBe expectedState
      onDone.dispose()
    }
  }

  it should "return yield None with the state Complete if the counter is one and inner stage returns yield None" in {
    val stage = mock[Stage[String, Long, Nothing]]
    val onDone = mock[OnDone[String, Long, Nothing]]
    (stage.apply _).expects("xi").returns(Yield.None(onDone))
    inside(Countdown.Impl(1, 3, stage).apply("xi")) { case Yield.None(completed) =>
      testCompletedOnDone(onDone, completed)
    }
  }

  it should "return yield None with the state Complete if the counter is one and inner stage returns yield Some" in {
    val stage = mock[Stage[String, Long, Nothing]]
    val onDone = mock[OnDone[String, Long, Nothing]]
    (stage.apply _).expects("xi").returns(Yield.Some(42, onDone))
    inside(Countdown.Impl(1, 3, stage).apply("xi")) { case Yield.Some(42, completed) =>
      testCompletedOnDone(onDone, completed)
    }
  }

  private def testCompletedOnDone(
      onDone: OnDone[String, Long, Nothing],
      completed: OnDone[String, Long, Nothing]
  ): Unit = {
    val stage = mock[Stage[String, Long, Nothing]]
    val state = State.Success(stage)
    val expectedState = State.Complete(Countdown.Impl(3, 3, stage))
    (onDone.onComplete _).expects().returns(state)
    completed.onSuccess() shouldBe expectedState
    (onDone.onComplete _).expects().returns(state)
    completed.onComplete() shouldBe expectedState
    (onDone.onError _).expects().returns(state)
    completed.onError() shouldBe expectedState
    (onDone.onPanic _).expects().returns(state)
    completed.onPanic() shouldBe expectedState
    (onDone.dispose _).expects()
    completed.dispose()
  }

  it should "return the state Complete after 3 iterations" in {
    val stage1 = mock[Stage[String, Long, Nothing]]
    val initial = Countdown(3, stage1)
    val onDone1 = mock[OnDone[String, Long, Nothing]]
    (stage1.apply _).expects("xi").returns(Yield.Some(3, onDone1))

    val (cdOnDone1, stage2) = inside(initial("xi")) { case Yield.Some(3, onDone) =>
      val stage = mock[Stage[String, Long, Nothing]]
      (onDone1.onSuccess _).expects().returns(State.Success(stage))
      (onDone, stage)
    }

    val (cdStage1, onDone2) = inside(cdOnDone1.onSuccess()) { case State.Success(stage) =>
      stage shouldBe a[Countdown.Impl[?, ?, ?]]
      val onDone = mock[OnDone[String, Long, Nothing]]
      (stage2.apply _).expects("query").returns(Yield.Some(17, onDone))
      (stage, onDone)
    }

    val (cdOnDone2, stage3) = inside(cdStage1("query")) { case Yield.Some(17, onDone) =>
      val stage = mock[Stage[String, Long, Nothing]]
      (onDone2.onSuccess _).expects().returns(State.Success(stage))
      (onDone, stage)
    }

    val (cdStage2, onDone3) = inside(cdOnDone2.onSuccess()) { case State.Success(stage) =>
      stage shouldBe a[Countdown.Impl[?, ?, ?]]
      val onDone = mock[OnDone[String, Long, Nothing]]
      (stage3.apply _).expects("language").returns(Yield.Some(42, onDone))
      (stage, onDone)
    }

    inside(cdStage2("language")) { case Yield.Some(42, onDone) =>
      // onDone.onSuccess() shouldBe State.Complete()
      (onDone3.dispose _).expects()
      onDone.dispose()
    }
  }
//
//  it should "return the state Complete on complete" in { testCompletedState(_.onComplete()) }
//  it should "return the state Complete on error" in { testCompletedState(_.onError()) }
//  it should "return the state Complete on panic" in { testCompletedState(_.onPanic()) }
//
//  private def testCompletedState(call: OnDone[Int, Int, Nothing] => State[Int, Int, Nothing]): Assertion = {
//    val initial = Countdown[Int](3, global = false)
//    inside(initial(42)) { case Yield.Some(42, onDone) =>
//      inside(onDone.onSuccess()) { case State.Success(stage) =>
//        inside(stage(17)) { case Yield.Some(17, onDone) =>
//          call(onDone) should matchPattern { case State.Complete(`initial`) => }
//        }
//      }
//    }
//  }
}
