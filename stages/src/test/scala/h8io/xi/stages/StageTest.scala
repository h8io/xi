package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StageTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Safe stage `apply` method" should "not throw an error if it was created from common stage" in {
    val expectedException = new Exception("stage failure")
    val stage: Stage[Unit, Nothing, Nothing] = _ => throw expectedException
    the[Exception] thrownBy stage(()) shouldBe expectedException
    inside(stage.safe(())) { case Yield.None(onDone) =>
      val expectedState = State.failure(expectedException)
      onDone.onSuccess() shouldBe expectedState
      onDone.onComplete() shouldBe expectedState
      onDone.onFailure() shouldBe expectedState
    }
  }

  it should "throw an exception if it is unsafe even with safe method" in {
    val expectedException = new Exception("safe stage failure")
    val stage: Stage.Safe[Unit, Nothing, Nothing] = _ => throw expectedException
    the[Exception] thrownBy stage(()) shouldBe expectedException
    the[Exception] thrownBy stage.safe(()) shouldBe expectedException
  }

  "Composed stage `apply` method" should "should not call the second stage if the first one returns Yield.None" in {
    val stage1: Stage[Unit, Int, Nothing] = _ => Yield.None(mock[OnDone[Unit, Int, Nothing]])
    val stage2 = mock[Stage[Int, Unit, Nothing]]
    (stage1 ~> stage2)(()) shouldBe a[Yield.None[?, ?, ?]]
  }

  it should "call the second stage if the first one returns Yield.Some and the result should be Yield.None" in {
    val stage1: Stage[Unit, Int, Nothing] = _ => Yield.Some(42, mock[OnDone[Unit, Int, Nothing]])
    val stage2 = mock[Stage[Int, Unit, Nothing]]
    (stage2.apply _).expects(42).returns(Yield.None(mock[OnDone[Int, Unit, Nothing]]))
    (stage1 ~> stage2)(()) shouldBe a[Yield.None[?, ?, ?]]
  }

  it should "call the second stage if the first one returns Yield.Some and the result should be Yield.Some" in {
    val stage1: Stage[Unit, Int, Nothing] = _ => Yield.Some(42, mock[OnDone[Unit, Int, Nothing]])
    val stage2 = mock[Stage[Int, Short, Nothing]]
    (stage2.apply _).expects(42).returns(Yield.Some(17, mock[OnDone[Int, Short, Nothing]]))
    (stage1 ~> stage2)(()) should matchPattern { case Yield.Some(17, _) => }
  }
}
