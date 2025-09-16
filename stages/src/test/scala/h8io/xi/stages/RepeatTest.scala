package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RepeatTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Repeat" should "be executed until the state `Complete`" in {
    val stage1 = mock[Stage[String, Int, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    val stage3 = mock[Stage[String, Int, Nothing]]
    val stage4 = mock[Stage[String, Int, Nothing]]
    val stage5 = mock[Stage[String, Int, Nothing]]
    val onDone1 = mock[OnDone[String, Int, Nothing]]
    val onDone2 = mock[OnDone[String, Int, Nothing]]
    val onDone3 = mock[OnDone[String, Int, Nothing]]
    val onDone4 = mock[OnDone[String, Int, Nothing]]
    val onDone5 = mock[OnDone[String, Int, Nothing]]
    val state1 = State.Success(stage2)
    val state2 = State.Success(stage3)
    val state3 = State.Complete(stage4)
    val state4 = State.Success(stage5)
    val state5 = State.Complete(mock[Stage[String, Int, Nothing]])
    inSequence {
      (stage1.apply _).expects("xi").returns(Yield.Some(3, onDone1))
      (onDone1.onSuccess _).expects().returns(state1)
      (stage2.apply _).expects("xi").returns(Yield.None(onDone2))
      (onDone2.onSuccess _).expects().returns(state2)
      (stage3.apply _).expects("xi").returns(Yield.Some(42, onDone3))
      (onDone3.onSuccess _).expects().returns(state3)
    }
    inside(Repeat(stage1)("xi")) { case Yield.Some(42, onDone) =>
      inside(onDone.onSuccess()) { case State.Success(stage) =>
        stage shouldBe a[Repeat[?, ?, ?]]
        inSequence {
          (stage4.apply _).expects("ql").returns(Yield.Some(17, onDone4))
          (onDone4.onSuccess _).expects().returns(state4)
          (stage5.apply _).expects("ql").returns(Yield.None(onDone5))
          (onDone5.onSuccess _).expects().returns(state5)
        }
        inside(stage("ql")) { case Yield.None(onDone) =>
          (onDone5.dispose _).expects()
          onDone.dispose()
        }
      }
    }
  }

  it should "be executed until the state `Failure`" in {}
}
