package h8io.xi.stages

import cats.data.NonEmptyChain
import State.Complete
import org.scalamock.handlers.CallHandler0
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YieldTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Yield.Some" should "be successfully composed with stage that returns Yield.Some" in {
    val onDone1 = mock[OnDone[Unit, Int, Nothing]]
    val onDone2 = mock[OnDone[Int, Int, Nothing]]
    val stage2 = mock[Stage[Int, Int, Nothing]]
    (stage2.apply _).expects(17).returns(Yield.Some(42, onDone2))
    inside(Yield.Some(17, onDone1) ~> stage2) { case Yield.Some(42, onDone) =>
      (onDone2.onSuccess _).expects().returns(State.Success(mock[Stage[Int, Int, Nothing]]))
      (onDone1.onSuccess _).expects().returns(State.Success(mock[Stage[Unit, Int, Nothing]]))
      onDone.onSuccess() shouldBe a[State.Success[?, ?, ?]]
    }
  }

  it should "be successfully composed with stage that returns Yield.None" in {
    val onDone1 = mock[OnDone[Unit, String, Nothing]]
    val onDone2 = mock[OnDone[String, Int, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    (stage2.apply _).expects("xi").returns(Yield.None(onDone2))
    inside(Yield.Some("xi", onDone1) ~> stage2) { case Yield.None(onDone) =>
      inSequence {
        (onDone2.onSuccess _).expects().returns(State.Success(mock[Stage[String, Int, Nothing]]))
        (onDone1.onSuccess _).expects().returns(State.Complete)
      }
      onDone.onSuccess() shouldBe Complete
    }
  }

  "Yield.None" should "be successfully composed with any stage" in {
    val onDone1 = mock[OnDone[Unit, String, String]]
    val stage2 = mock[Stage[String, Int, String]]
    inSequence { (onDone1.onSuccess _).expects().returns(State.error("error")) }
    inside(Yield.None(onDone1) ~> stage2) { case Yield.None(onDone) =>
      inside(onDone.onSuccess()) { case State.Failure(failures) => failures shouldBe NonEmptyChain.one(Right("error")) }
    }
  }

  "Composed Yield.None" should "produce a correct state on success" in {
    testCombined(mockOnDone => (mockOnDone.onSuccess _).expects(), _.onSuccess())
  }

  it should "produce a correct state on complete" in {
    testCombined(mockOnDone => (mockOnDone.onComplete _).expects(), _.onComplete())
  }

  it should "produce a correct state on failure" in {
    testCombined(mockOnDone => (mockOnDone.onFailure _).expects(), _.onFailure())
  }

  private def testCombined(
      mockCall: OnDone[Unit, String, Nothing] => CallHandler0[State[Unit, String, Nothing]],
      call: OnDone[Unit, Int, Nothing] => State[Unit, Int, Nothing]
  ): Unit = {
    val onDone1 = mock[OnDone[Unit, String, Nothing]]
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    mockCall(onDone1).returns(State.Success(stage1))
    inside(Yield.None(onDone1) ~> stage2) { case Yield.None(onDone) =>
      inside(call(onDone)) { case State.Success(stage) =>
        inSequence {
          (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
          (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
        }
        stage(()) should matchPattern { case Yield.Some(42, _) => }
      }
    }
  }

  "Yield.Some.with" should "replace OnDone object" in {
    val onDone1 = mock[OnDone[String, Int, Nothing]]
    val onDone2 = mock[OnDone[String, Int, Nothing]]
    onDone1 shouldNot equal(onDone2)
    Yield.Some(42, onDone1) `with` onDone2 shouldEqual Yield.Some(42, onDone2)
  }

  "Yield.None.with" should "replace OnDone object" in {
    val onDone1 = mock[OnDone[String, Int, Nothing]]
    val onDone2 = mock[OnDone[String, Int, Nothing]]
    onDone1 shouldNot equal(onDone2)
    Yield.None(onDone1) `with` onDone2 shouldEqual Yield.None(onDone2)
  }
}
