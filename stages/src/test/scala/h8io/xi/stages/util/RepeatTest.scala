package h8io.xi.stages.util

import cats.data.NonEmptyChain
import h8io.xi.stages.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}

class RepeatTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Repeat" should "be executed while condition is true" in {
    val condition1 = mock[Condition]("Condition 1")
    val condition2 = mock[Condition]("Condition 2")
    val condition3 = mock[Condition]("Condition 3")
    val stage1 = mock[Stage[Int, String, Nothing]]("Stage 1")
    val stage2 = mock[Stage[Int, String, Nothing]]("Stage 2")
    val onDone1 = mock[OnDone[Int, String, Nothing]]("OnDone 1")
    val state1 = State.Success(stage2)
    val reset1 = mock[() => Condition]("Condition.reset 1")

    inSequence {
      (() => condition1.check).expects().returns(true)
      (stage1.apply _).expects(-1).returns(Yield.Some("xi", onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (condition1.advance _).expects().returns(condition2)
      (() => condition2.check).expects().returns(false)
      (() => condition2.reset).expects().returns(reset1)
    }

    def checkState(
        state: State[Int, String, Nothing],
        reset: () => Condition,
        stage: Stage[Int, String, Nothing]): Assertion =
      state should matchPattern { case State.Success(Repeat(`reset`, `stage`)) => }

    val repeat = inside(Repeat(() => condition1, stage1)(-1)) { case Yield.Some("xi", onDone) =>
      val state = onDone.onSuccess()
      checkState(state, reset1, stage2)
      checkState(onDone.onComplete(), reset1, stage2)
      checkState(onDone.onError(), reset1, stage2)
      checkState(onDone.onPanic(), reset1, stage2)
      (onDone1.dispose _).expects()
      onDone.dispose()
      inside(state) { case State.Success(stage) => stage }
    }

    val reset2 = mock[() => Condition]("Condition.reset 2")
    inSequence {
      (reset1.apply _).expects().returns(condition3)
      (() => condition3.check).expects().returns(false)
      (() => condition3.reset).expects().returns(reset2)
    }

    inside(repeat(13)) { case Yield.None(onDone) =>
      checkState(onDone.onSuccess(), reset2, stage2)
      checkState(onDone.onComplete(), reset2, stage2)
      checkState(onDone.onError(), reset2, stage2)
      checkState(onDone.onPanic(), reset2, stage2)
      onDone.dispose()
    }

  }

  it should "be executed until the state is Complete" in {
    val stage1 = mock[Stage[String, Int, Nothing]]("Stage 1")
    val stage2 = mock[Stage[String, Int, Nothing]]("Stage 2")
    val stage3 = mock[Stage[String, Int, Nothing]]("Stage 3")
    val stage4 = mock[Stage[String, Int, Nothing]]("Stage 4")
    val stage5 = mock[Stage[String, Int, Nothing]]("Stage 5")
    val stage6 = mock[Stage[String, Int, Nothing]]("Stage 6")
    val onDone1 = mock[OnDone[String, Int, Nothing]]("OnDone 1")
    val onDone2 = mock[OnDone[String, Int, Nothing]]("OnDone 2")
    val onDone3 = mock[OnDone[String, Int, Nothing]]("OnDone 3")
    val onDone4 = mock[OnDone[String, Int, Nothing]]("OnDone 4")
    val onDone5 = mock[OnDone[String, Int, Nothing]]("OnDone 5")
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val state3 = State.Complete(stage4)
    val state4 = State.Success(stage5)
    val state5 = State.Complete(stage6)

    inSequence {
      (stage1.apply _).expects("xi").returns(Yield.Some(3, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects("xi").returns(Yield.None(onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects("xi").returns(Yield.Some(42, onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    val repeat = inside(Repeat(stage1)("xi")) { case Yield.Some(42, onDone) =>
      inside(onDone.onSuccess()) { case State.Success(stage) =>
        stage shouldBe Repeat(Condition.True, stage4)
        stage
      }
    }

    inSequence {
      (stage4.apply _).expects("ql").returns(Yield.Some(17, onDone4))
      (onDone4.onSuccess _).expects().returns(state4)
      (stage5.apply _).expects("ql").returns(Yield.None(onDone5))
      (onDone5.onSuccess _).expects().returns(state5)
    }
    inside(repeat("ql")) { case Yield.None(onDone) =>
      inside(onDone.onSuccess()) { case State.Success(stage) =>
        stage shouldBe Repeat(Condition.True, stage6)
        (onDone5.dispose _).expects()
        onDone.dispose()
      }
    }
  }

  it should "be executed until the state is Error" in {
    val stage1 = mock[Stage[String, Int, String]]("Stage 1")
    val stage2 = mock[Stage[String, Int, String]]("Stage 2")
    val stage3 = mock[Stage[String, Int, String]]("Stage 3")
    val stage4 = mock[Stage[String, Int, String]]("Stage 4")
    val stage5 = mock[Stage[String, Int, String]]("Stage 5")
    val stage6 = mock[Stage[String, Int, String]]("Stage 6")
    val onDone1 = mock[OnDone[String, Int, String]]("OnDone 1")
    val onDone2 = mock[OnDone[String, Int, String]]("OnDone 2")
    val onDone3 = mock[OnDone[String, Int, String]]("OnDone 3")
    val onDone4 = mock[OnDone[String, Int, String]]("OnDone 4")
    val onDone5 = mock[OnDone[String, Int, String]]("OnDone 5")
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val state3 = State.Error(stage4, "the first error")
    val state4 = State.Success(stage5)
    val state5 = State.Error(stage6, "the second error")

    inSequence {
      (stage1.apply _).expects("xi").returns(Yield.Some(3, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects("xi").returns(Yield.None(onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects("xi").returns(Yield.Some(42, onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    val repeat = inside(Repeat(stage1)("xi")) { case Yield.Some(42, onDone) =>
      inside(onDone.onSuccess()) { case State.Error(stage, errors) =>
        errors shouldBe NonEmptyChain("the first error")
        stage shouldBe Repeat(Condition.True, stage4)
        stage
      }
    }

    inSequence {
      (stage4.apply _).expects("ql").returns(Yield.Some(17, onDone4))
      (onDone4.onSuccess _).expects().returns(state4)
      (stage5.apply _).expects("ql").returns(Yield.None(onDone5))
      (onDone5.onSuccess _).expects().returns(state5)
    }
    inside(repeat("ql")) { case Yield.None(onDone) =>
      inside(onDone.onSuccess()) { case State.Error(stage, errors) =>
        errors shouldBe NonEmptyChain("the second error")
        stage shouldBe Repeat(Condition.True, stage6)
        (onDone5.dispose _).expects()
        onDone.dispose()
      }
    }
  }

  it should "be executed until the state is Panic" in {
    val stage1 = mock[Stage[String, Int, Nothing]]("Stage 1")
    val stage2 = mock[Stage[String, Int, Nothing]]("Stage 2")
    val stage3 = mock[Stage[String, Int, Nothing]]("Stage 3")
    val onDone1 = mock[OnDone[String, Int, Nothing]]("OnDone 1")
    val onDone2 = mock[OnDone[String, Int, Nothing]]("OnDone 2")
    val onDone3 = mock[OnDone[String, Int, Nothing]]("OnDone 3")
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val expectedCause = new Exception
    val state3 = State.Panic(expectedCause)
    inSequence {
      (stage1.apply _).expects("xi").returns(Yield.Some(3, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects("xi").returns(Yield.None(onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects("xi").returns(Yield.Some(42, onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    inside(Repeat(stage1)("xi")) { case Yield.Some(42, onDone) =>
      inside(onDone.onSuccess()) { case State.Panic(causes) =>
        causes shouldBe NonEmptyChain(expectedCause)
        (onDone3.dispose _).expects()
        onDone.dispose()
      }
    }
  }

  "apply without condition" should "return an infinite loop" in {
    val stage = mock[Stage[Int, Unit, Nothing]]
    Repeat(stage) shouldBe Repeat(Condition.True, stage)
  }
}
