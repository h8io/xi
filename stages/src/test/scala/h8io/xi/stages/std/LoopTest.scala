package h8io.xi.stages.std

import cats.data.NonEmptyChain
import h8io.xi.stages.{OnDone, Stage, State, Yield}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LoopTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Loop" should "be executed until the result is Yield.None" in {
    val stage1 = mock[Stage[Int, Int, Nothing]]
    val stage2 = mock[Stage[Int, Int, Nothing]]
    val stage3 = mock[Stage[Int, Int, Nothing]]
    val stage4 = mock[Stage[Int, Int, Nothing]]
    val stage5 = mock[Stage[Int, Int, Nothing]]
    val onDone1 = mock[OnDone[Int, Int, Nothing]]
    val onDone2 = mock[OnDone[Int, Int, Nothing]]
    val onDone3 = mock[OnDone[Int, Int, Nothing]]
    val onDone4 = mock[OnDone[Int, Int, Nothing]]
    val onDone5 = mock[OnDone[Int, Int, Nothing]]
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val state3 = State.Success(stage4)
    val state4 = State.Success(stage5)
    val state5 = State.Success(mock[Stage[Int, Int, Nothing]])
    inSequence {
      (stage1.apply _).expects(1).returns(Yield.Some(2, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects(2).returns(Yield.Some(6, onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects(6).returns(Yield.None(onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    inside(Loop(stage1)(1)) { case Yield.None(onDone) =>
      inside(onDone.onSuccess()) { case State.Success(stage) =>
        stage shouldBe a[Loop[?, ?]]
        inSequence {
          (stage4.apply _).expects(7).returns(Yield.Some(35, onDone4))
          (onDone4.onSuccess _).expects().returns(state4)
          (stage5.apply _).expects(35).returns(Yield.None(onDone5))
          (onDone5.onSuccess _).expects().returns(state5)
        }
        inside(stage(7)) { case Yield.None(onDone) =>
          inside(onDone.onSuccess()) { case State.Success(stage) =>
            stage shouldBe a[Loop[?, ?]]
            (onDone5.dispose _).expects()
            onDone.dispose()
          }
        }
      }
    }
  }

  it should "be executed until the state is Complete" in {
    val stage1 = mock[Stage[Int, Int, Nothing]]
    val stage2 = mock[Stage[Int, Int, Nothing]]
    val stage3 = mock[Stage[Int, Int, Nothing]]
    val stage4 = mock[Stage[Int, Int, Nothing]]
    val stage5 = mock[Stage[Int, Int, Nothing]]
    val onDone1 = mock[OnDone[Int, Int, Nothing]]
    val onDone2 = mock[OnDone[Int, Int, Nothing]]
    val onDone3 = mock[OnDone[Int, Int, Nothing]]
    val onDone4 = mock[OnDone[Int, Int, Nothing]]
    val onDone5 = mock[OnDone[Int, Int, Nothing]]
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val state3 = State.Complete(stage4)
    val state4 = State.Success(stage5)
    val state5 = State.Complete(mock[Stage[Int, Int, Nothing]])
    inSequence {
      (stage1.apply _).expects(1).returns(Yield.Some(2, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects(2).returns(Yield.Some(6, onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects(6).returns(Yield.Some(24, onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    inside(Loop(stage1)(1)) { case Yield.Some(24, onDone) =>
      inside(onDone.onSuccess()) { case State.Success(stage) =>
        stage shouldBe a[Loop[?, ?]]
        inSequence {
          (stage4.apply _).expects(7).returns(Yield.Some(35, onDone4))
          (onDone4.onSuccess _).expects().returns(state4)
          (stage5.apply _).expects(35).returns(Yield.None(onDone5))
          (onDone5.onSuccess _).expects().returns(state5)
        }
        inside(stage(7)) { case Yield.None(onDone) =>
          inside(onDone.onSuccess()) { case State.Success(stage) =>
            stage shouldBe a[Loop[?, ?]]
            (onDone5.dispose _).expects()
            onDone.dispose()
          }
        }
      }
    }
  }

  it should "be executed until the state is Error" in {
    val stage1 = mock[Stage[Int, Int, String]]
    val stage2 = mock[Stage[Int, Int, String]]
    val stage3 = mock[Stage[Int, Int, String]]
    val stage4 = mock[Stage[Int, Int, String]]
    val stage5 = mock[Stage[Int, Int, String]]
    val onDone1 = mock[OnDone[Int, Int, String]]
    val onDone2 = mock[OnDone[Int, Int, String]]
    val onDone3 = mock[OnDone[Int, Int, String]]
    val onDone4 = mock[OnDone[Int, Int, String]]
    val onDone5 = mock[OnDone[Int, Int, String]]
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val state3 = State.Error(stage4, "the first error")
    val state4 = State.Success(stage5)
    val state5 = State.Error(mock[Stage[Int, Int, String]], "the second error")
    inSequence {
      (stage1.apply _).expects(1).returns(Yield.Some(1, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects(1).returns(Yield.Some(2, onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects(2).returns(Yield.Some(3, onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    inside(Loop(stage1)(1)) { case Yield.Some(3, onDone) =>
      inside(onDone.onSuccess()) { case State.Error(stage, errors) =>
        errors shouldBe NonEmptyChain("the first error")
        stage shouldBe a[Loop[?, ?]]
        inSequence {
          (stage4.apply _).expects(5).returns(Yield.Some(8, onDone4))
          (onDone4.onSuccess _).expects().returns(state4)
          (stage5.apply _).expects(8).returns(Yield.None(onDone5))
          (onDone5.onSuccess _).expects().returns(state5)
        }
        inside(stage(5)) { case Yield.None(onDone) =>
          inside(onDone.onSuccess()) { case State.Error(stage, errors) =>
            errors shouldBe NonEmptyChain("the second error")
            stage shouldBe a[Loop[?, ?]]
            (onDone5.dispose _).expects()
            onDone.dispose()
          }
        }
      }
    }
  }

  it should "be executed until the state is Panic" in {
    val stage1 = mock[Stage[Int, Int, Nothing]]
    val stage2 = mock[Stage[Int, Int, Nothing]]
    val stage3 = mock[Stage[Int, Int, Nothing]]
    val onDone1 = mock[OnDone[Int, Int, Nothing]]
    val onDone2 = mock[OnDone[Int, Int, Nothing]]
    val onDone3 = mock[OnDone[Int, Int, Nothing]]
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val expectedCause = new Exception
    val state3 = State.Panic(expectedCause)
    inSequence {
      (stage1.apply _).expects(1).returns(Yield.Some(2, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects(2).returns(Yield.Some(3, onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects(3).returns(Yield.Some(4, onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    inside(Loop(stage1)(1)) { case Yield.Some(4, onDone) =>
      inside(onDone.onSuccess()) { case State.Panic(causes) =>
        causes shouldBe NonEmptyChain(expectedCause)
        (onDone3.dispose _).expects()
        onDone.dispose()
      }
    }
  }
}
