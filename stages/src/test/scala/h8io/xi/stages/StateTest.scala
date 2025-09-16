package h8io.xi.stages

import cats.data.NonEmptyChain
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Success" should "become a Success if it is composed with a Success" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Success(stage1) <~ State.Success(stage2)) { case State.Success(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "become the Complete if it is composed with the Complete" in {
    State.Success(mock[Stage[Int, String, Nothing]]) <~ State.Complete shouldBe State.Complete
  }

  it should "become a Failure if it is composed with a Failure" in {
    val failure = State.error("Success <~ Failure")
    State.Success(mock[Stage[String, Int, Nothing]]) <~ failure shouldBe failure
  }

  it should "call onSuccess in composition with OnDone" in {
    val onDone = mock[OnDone[Boolean, String, Nothing]]
    (onDone.onSuccess _).expects().returns(State.Complete)
    State.Success(mock[Stage[String, Int, Nothing]]) ~> onDone shouldBe State.Complete
  }

  "Complete" should "remain the Complete if it is composed with a Success" in {
    State.Complete <~ State.Success(mock[Stage[Boolean, Byte, Nothing]]) shouldBe State.Complete
  }

  it should "remain the Complete if it is composed with the Complete" in {
    State.Complete <~ State.Complete shouldBe State.Complete
  }

  it should "become a Failure if it is composed with a Failure" in {
    val failure = State.failure(new Exception("Complete <~ Failure"))
    State.Complete <~ failure shouldBe failure
  }

  it should "call onComplete in composition with OnDone" in {
    val onDone = mock[OnDone[Boolean, String, Nothing]]
    (onDone.onComplete _).expects().returns(State.Complete)
    State.Complete ~> onDone shouldBe State.Complete
  }

  "Failure" should "remain the same Failure if it is composed with a Success" in {
    val failure = State.failure(new Exception("Failure <~ Success"))
    failure <~ State.Success(mock[Stage[Boolean, Byte, Nothing]]) shouldBe failure
  }

  it should "remain the Complete if it is composed with the Complete" in {
    val failure = State.failure(new Exception("Failure <~ Complete"))
    failure <~ State.Complete shouldBe failure
  }

  it should "become a composed Failure if it is composed with a Failure" in {
    val failure1 = State.failure(new Exception("Complete <~ Failure"))
    val failure2 = State.error("Complete <~ Failure")
    failure1 <~ failure2 shouldBe State.Failure(failure2.failures ++ failure1.failures)
  }

  it should "call onComplete in composition with OnDone" in {
    val failure = State.failure(new Exception("Failure ~> OnDone"))
    val onDone = mock[OnDone[Boolean, String, Nothing]]
    (onDone.onFailure _).expects().returns(State.Complete)
    failure ~> onDone shouldBe failure
  }

  "error" should "create an errors Failure" in {
    State.error("xi", "query", "language") shouldBe
      State.Failure(NonEmptyChain(Right("xi"), Right("query"), Right("language")))
  }

  "failure" should "return a correct Failure object" in {
    val exception = new Exception("We are failed")
    State.failure(exception) shouldBe State.Failure(NonEmptyChain(Left(exception)))
  }
}
