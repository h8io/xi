package h8io.xi.stages

import cats.data.NonEmptyChain
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}

class StateTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "onDone" should "return a stable OnDone object" in {
    testOnDone(State.Success(mock[Stage[Unit, Unit, Nothing]]))
    testOnDone(State.Complete(mock[Stage[Unit, Unit, Nothing]]))
    testOnDone(State.Error(mock[Stage[Unit, Unit, Nothing]], "error message"))
    testOnDone(State.Panic(new Exception))
  }

  private def testOnDone[I, O, E](state: State[I, O, E]): Unit = {
    val dispose = mock[() => Unit]
    val onDone = state.onDone(dispose)
    onDone.onSuccess() shouldBe state
    onDone.onComplete() shouldBe state
    onDone.onError() shouldBe state
    onDone.onPanic() shouldBe state
    (dispose.apply _).expects()
    onDone.dispose()
  }

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
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Success(stage1) <~ State.Complete(stage2)) { case State.Complete(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "become an Error if it is composed with a Error" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Success(stage1) <~ State.Error(stage2, "Success <~ Error")) { case State.Error(stage, errors) =>
      errors shouldBe NonEmptyChain("Success <~ Error")
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "become a Panic if it is composed with a Panic" in {
    val panic = State.Panic(new Exception)
    State.Success(mock[Stage[String, Int, Nothing]]) <~ panic shouldBe panic
  }

  it should "call onSuccess in composition with OnDone" in {
    val onDone = mock[OnDone.Safe[String, Int, Nothing]]
    val stage1 = mock[Stage[String, Int, Nothing]]
    val stage2 = mock[Stage[Int, Boolean, Nothing]]
    (onDone.onComplete _).expects().returns(State.Complete(stage1))
    inside(State.Complete(stage2) ~> onDone) { case State.Complete(stage) =>
      inSequence {
        (stage1.apply _).expects("xi").returns(Yield.Some(42, onDone))
        (stage2.apply _).expects(42).returns(Yield.Some(true, mock[OnDone[Int, Boolean, Nothing]]))
      }
      stage("xi") should matchPattern { case Yield.Some(true, _) => }
    }
  }

  "Complete" should "remain a Complete if it is composed with a Success" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Complete(stage1) <~ State.Success(stage2)) { case State.Complete(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "become a Complete if it is composed with the Complete" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Complete(stage1) <~ State.Complete(stage2)) { case State.Complete(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "become an Error if it is composed with a Error" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Complete(stage1) <~ State.Error(stage2, "Complete <~ Error")) { case State.Error(stage, errors) =>
      errors shouldBe NonEmptyChain("Complete <~ Error")
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "become a Panic if it is composed with a Panic" in {
    val panic = State.Panic(new Exception)
    State.Complete(mock[Stage[Unit, String, Nothing]]) <~ panic shouldBe panic
  }

  it should "call onComplete in composition with OnDone" in {
    val onDone = mock[OnDone.Safe[Boolean, String, Nothing]]
    val stage1 = mock[Stage[Boolean, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    (onDone.onComplete _).expects().returns(State.Complete(stage1))
    inside(State.Complete(stage2) ~> onDone) { case State.Complete(stage) =>
      inSequence {
        (stage1.apply _).expects(true).returns(Yield.Some("xi", onDone))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(true) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  "Error" should "remain an Error if it is composed with a Success" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Error(stage1, "Error <~ Success") <~ State.Success(stage2)) { case State.Error(stage, errors) =>
      errors shouldBe NonEmptyChain.one("Error <~ Success")
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "remain an Error if it is composed with the Complete" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Error(stage1, "Error <~ Complete") <~ State.Complete(stage2)) { case State.Error(stage, errors) =>
      errors shouldBe NonEmptyChain.one("Error <~ Complete")
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "remain an Error if it is composed with a Error" in {
    val stage1 = mock[Stage[Unit, String, Nothing]]
    val stage2 = mock[Stage[String, Int, Nothing]]
    inside(State.Error(stage1, "Error <~ Error (1)") <~ State.Error(stage2, "Error <~ Error (2)")) {
      case State.Error(stage, errors) =>
        errors shouldBe NonEmptyChain("Error <~ Error (1)", "Error <~ Error (2)")
        inSequence {
          (stage1.apply _).expects(()).returns(Yield.Some("xi", mock[OnDone[Unit, String, Nothing]]))
          (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
        }
        stage(()) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  it should "become a Panic if it is composed with a Panic" in {
    val panic = State.Panic(new Exception)
    State.Error(mock[Stage[Unit, String, Nothing]], "Error <~ Panic") <~ panic shouldBe panic
  }

  it should "call onError in composition with OnDone" in {
    val onDone = mock[OnDone.Safe[Boolean, String, String]]
    val stage1 = mock[Stage[Boolean, String, String]]
    val stage2 = mock[Stage[String, Int, String]]
    (onDone.onError _).expects().returns(State.Error(stage1, "Error ~> onDone (1)"))
    inside(State.Error(stage2, "Error ~> onDone (2)") ~> onDone) { case State.Error(stage, errors) =>
      errors shouldBe NonEmptyChain("Error ~> onDone (1)", "Error ~> onDone (2)")
      inSequence {
        (stage1.apply _).expects(true).returns(Yield.Some("xi", onDone))
        (stage2.apply _).expects("xi").returns(Yield.Some(42, mock[OnDone[String, Int, Nothing]]))
      }
      stage(true) should matchPattern { case Yield.Some(42, _) => }
    }
  }

  "Panic" should "remain the same Failure if it is composed with a Success" in {
    val panic = State.Panic(new Exception)
    panic <~ State.Success(mock[Stage[Boolean, Byte, Nothing]]) shouldBe panic
  }

  it should "remain the same Failure if it is composed with the Complete" in {
    val panic = State.Panic(new Exception)
    panic <~ State.Complete(mock[Stage[Int, String, Nothing]]) shouldBe panic
  }

  it should "remain a Panic if it is composed with a Error" in {
    val panic = State.Panic(new Exception)
    panic <~ State.Error(mock[Stage[Double, Long, String]], "Panic <~ Error") shouldBe State.Panic(panic.exceptions)
  }

  it should "remain a Panic if it is composed with a Panic" in {
    val exception1 = new Exception("the first panic")
    val panic1 = State.Panic(exception1)
    val exception2 = new Exception("the second panic")
    val panic2 = State.Panic(exception2)
    panic1 <~ panic2 shouldBe State.Panic(NonEmptyChain(exception2, exception1))
  }

  it should "call onPanic in composition with OnDone" in {
    val failure = State.Panic(new Exception)
    val onDone = mock[OnDone.Safe[Boolean, String, Nothing]]
    (onDone.onPanic _).expects().returns(State.Complete(mock[Stage[Boolean, String, Nothing]]))
    failure ~> onDone shouldBe failure
  }

  "lift" should "update the stage for the state Success" in test(State.Success(_), _.map(_))

  it should "update the stage for the state Complete" in test(State.Complete(_), _.map(_))

  it should "update the stage for the state Error" in test(State.Error(_, "error"), _.map(_))

  it should "update the stage for the state Panic" in {
    val exception = new Exception
    test(_ => State.Panic(exception), _.map(_), isNotPanic = false)
  }

  "complete" should "return the stage Complete with updated stage for the state Success" in {
    val f = mock[Stage[Double, Int, String] => Stage[Double, Int, String]]
    val stage1 = mock[Stage[Double, Int, String]]
    val stage2 = mock[Stage[Double, Int, String]]
    (f.apply _).expects(stage1).returns(stage2)
    State.Success(stage1).complete(f) shouldBe State.Complete(stage2)
  }

  it should "update the stage for the state Complete" in test(State.Complete(_), _.complete(_))

  it should "update the stage for the state Error" in test(State.Error(_, "error"), _.complete(_))

  it should "update the stage for the state Panic" in {
    val exception = new Exception
    test(_ => State.Panic(exception), _.complete(_), isNotPanic = false)
  }

  private def test(
      create: Stage[Double, Int, String] => State[Double, Int, String],
      update: (State[Double, Int, String], Stage[Double, Int, String] => Stage[Double, Int, String]) => State[
        Double,
        Int,
        String
      ],
      isNotPanic: Boolean = true
  ): Assertion = {
    val f = mock[Stage[Double, Int, String] => Stage[Double, Int, String]]
    val stage1 = mock[Stage[Double, Int, String]]
    val stage2 = mock[Stage[Double, Int, String]]
    if (isNotPanic) (f.apply _).expects(stage1).returns(stage2)
    update(create(stage1), f) shouldBe create(stage2)
  }

  "Error" should "create a correct Error object" in {
    val stage = mock[Stage[Double, Unit, List[String]]]
    State.Error(stage, "xi") shouldBe State.Error(stage, NonEmptyChain("xi"))
  }

  "Panic" should "return a correct Panic object" in {
    val exception = new Exception
    State.Panic(exception) shouldBe State.Panic(NonEmptyChain(exception))
  }
}
