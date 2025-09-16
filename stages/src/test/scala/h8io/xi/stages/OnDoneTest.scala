package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.{Assertion, Inside}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnDoneTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Safe OnDone" should "not throw an exception with onSuccess" in {
    val onDone = mock[OnDone[Unit, Unit, Nothing]]
    val expectedException = new Exception("onSuccess exception")
    (onDone.onSuccess _).expects().throws(expectedException).twice()
    the[Exception] thrownBy onDone.onSuccess() shouldBe expectedException
    onDone.safe.onSuccess() shouldBe State.failure(expectedException)
  }

  it should "not throw an exception with onComplete" in {
    val onDone = mock[OnDone[Unit, Unit, Nothing]]
    val expectedException = new Exception("onComplete exception")
    (onDone.onComplete _).expects().throws(expectedException).twice()
    the[Exception] thrownBy onDone.onComplete() shouldBe expectedException
    onDone.safe.onComplete() shouldBe State.failure(expectedException)
  }

  it should "not throw an exception with onFailure" in {
    val onDone = mock[OnDone[Unit, Unit, Nothing]]
    val expectedException = new Exception("onFailure exception")
    (onDone.onFailure _).expects().throws(expectedException).twice()
    the[Exception] thrownBy onDone.onFailure() shouldBe expectedException
    onDone.safe.onFailure() shouldBe State.failure(expectedException)
  }

  "Composed OnDone.Safe" should "return a correct state with onSuccess" in {
    val onDone1 = mock[OnDone.Safe[Unit, Int, Nothing]]
    val stage1 = mock[Stage[Unit, Int, Nothing]]
    val onDone2 = mock[OnDone.Safe[Int, String, Nothing]]
    val stage2 = mock[Stage[Int, String, Nothing]]
    inSequence {
      (onDone2.onSuccess _).expects().returns(State.Success(stage2))
      (onDone1.onSuccess _).expects().returns(State.Success(stage1))
    }
    inside((onDone1 <~ onDone2).onSuccess()) { case State.Success(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some(42, mock[OnDone[Unit, Int, Nothing]]))
        (stage2.apply _).expects(42).returns(Yield.Some("xi", mock[OnDone[Int, String, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some("xi", _) => }
    }
  }

  it should "return a correct state with onComplete" in {
    val onDone1 = mock[OnDone.Safe[Unit, Int, Nothing]]
    val stage1 = mock[Stage[Unit, Int, Nothing]]
    val onDone2 = mock[OnDone.Safe[Int, String, Nothing]]
    val stage2 = mock[Stage[Int, String, Nothing]]
    inSequence {
      (onDone2.onComplete _).expects().returns(State.Success(stage2))
      (onDone1.onSuccess _).expects().returns(State.Success(stage1))
    }
    inside((onDone1 <~ onDone2).onComplete()) { case State.Success(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some(42, mock[OnDone[Unit, Int, Nothing]]))
        (stage2.apply _).expects(42).returns(Yield.Some("xi", mock[OnDone[Int, String, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some("xi", _) => }
    }
  }

  it should "return a correct state with onFailure" in {
    val onDone1 = mock[OnDone.Safe[Unit, Int, Nothing]]
    val stage1 = mock[Stage[Unit, Int, Nothing]]
    val onDone2 = mock[OnDone.Safe[Int, String, Nothing]]
    val stage2 = mock[Stage[Int, String, Nothing]]
    inSequence {
      (onDone2.onFailure _).expects().returns(State.Success(stage2))
      (onDone1.onSuccess _).expects().returns(State.Success(stage1))
    }
    inside((onDone1 <~ onDone2).onFailure()) { case State.Success(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some(42, mock[OnDone[Unit, Int, Nothing]]))
        (stage2.apply _).expects(42).returns(Yield.Some("xi", mock[OnDone[Int, String, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some("xi", _) => }
    }
  }

  "OnDone.of" should "return a success state" in {
    onDoneOfTest(State.Success(mock[Stage[Int, String, Int]]))
  }

  it should "return a complete state" in {
    onDoneOfTest(State.Complete)
  }

  it should "return a failure state" in {
    onDoneOfTest(State.error("error"))
  }

  private def onDoneOfTest[I, O, E](state: State[I, O, E]): Assertion = {
    val onDone = OnDone.of(state)
    onDone.onSuccess() shouldBe state
    onDone.onComplete() shouldBe state
    onDone.onFailure() shouldBe state
  }

  "OnDone.DoNothing" should "always return a success state" in {
    val stage = mock[Stage[Int, String, Nothing]]
    val onDone = OnDone.DoNothing(stage)
    val expectedState = State.Success(stage)
    onDone.onSuccess() shouldBe expectedState
    onDone.onComplete() shouldBe expectedState
    onDone.onFailure() shouldBe expectedState
  }

  "OnDone.OnFailure" should "always return a failure state" in {
    val expectedException = new Exception("Failure happens")
    val onDone = OnDone.OnFailure(expectedException)
    val expectedState = State.failure(expectedException)
    onDone.onSuccess() shouldBe expectedState
    onDone.onComplete() shouldBe expectedState
    onDone.onFailure() shouldBe expectedState
  }
}
