package h8io.xi.stages

import cats.data.NonEmptyChain
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OnDoneTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Safe OnDone" should "not throw an exception with onSuccess" in {
    val onDone = mock[OnDone[Unit, Unit, Nothing]]
    val expectedException = new Exception("onSuccess exception")
    (onDone.onSuccess _).expects().throws(expectedException).twice()
    the[Exception] thrownBy onDone.onSuccess() shouldBe expectedException
    onDone.safe.onSuccess() shouldBe State.Panic(expectedException)
    (onDone.dispose _).expects()
    onDone.safe.dispose()
  }

  it should "not throw an exception with onComplete" in {
    val onDone = mock[OnDone[Unit, Unit, Nothing]]
    val expectedException = new Exception("onComplete exception")
    (onDone.onComplete _).expects().throws(expectedException).twice()
    the[Exception] thrownBy onDone.onComplete() shouldBe expectedException
    onDone.safe.onComplete() shouldBe State.Panic(expectedException)
    (onDone.dispose _).expects()
    onDone.safe.dispose()
  }

  it should "not throw an exception with onError" in {
    val onDone = mock[OnDone[Unit, Unit, Nothing]]
    val expectedException = new Exception("onError exception")
    (onDone.onError _).expects().throws(expectedException).twice()
    the[Exception] thrownBy onDone.onError() shouldBe expectedException
    onDone.safe.onError() shouldBe State.Panic(expectedException)
    (onDone.dispose _).expects()
    onDone.safe.dispose()
  }

  it should "not throw an exception with onPanic" in {
    val onDone = mock[OnDone[Unit, Unit, Nothing]]
    val expectedException = new Exception("onPanic exception")
    (onDone.onPanic _).expects().throws(expectedException).twice()
    the[Exception] thrownBy onDone.onPanic() shouldBe expectedException
    onDone.safe.onPanic() shouldBe State.Panic(expectedException)
    (onDone.dispose _).expects()
    onDone.safe.dispose()
  }

  it should "return itself in safe method" in {
    val onDone = mock[OnDone.Safe[Unit, Unit, Nothing]]
    onDone.safe shouldBe onDone
  }

  "map" should "return mapped states" in {
    val stage1 = mock[Stage[String, Int, Boolean]]
    val stage2 = mock[Stage[Double, Float, String]]
    val exception = new Exception
    val mappedException = new RuntimeException
    val f: PartialFunction[State[String, Int, Boolean], State[Double, Float, String]] = {
      case State.Success(`stage1`) => State.Complete(stage2)
      case State.Complete(`stage1`) => State.Error(stage2, "error")
      case State.Error(`stage1`, errors) if errors == NonEmptyChain(true) => State.Panic(mappedException)
      case State.Panic(exceptions) if exceptions == NonEmptyChain(exception) => State.Success(stage2)
    }
    val onDone = mock[OnDone[String, Int, Boolean]]
    val mappedOnDone = onDone.map(f)
    (onDone.onSuccess _).expects().returns(State.Success(stage1))
    mappedOnDone.onSuccess() shouldBe State.Complete(stage2)
    (onDone.onComplete _).expects().returns(State.Complete(stage1))
    mappedOnDone.onComplete() shouldBe State.Error(stage2, "error")
    (onDone.onError _).expects().returns(State.Error(stage1, true))
    mappedOnDone.onError() shouldBe State.Panic(mappedException)
    (onDone.onPanic _).expects().returns(State.Panic(exception))
    mappedOnDone.onPanic() shouldBe State.Success(stage2)
    (onDone.dispose _).expects()
    mappedOnDone.dispose()
  }

  "complete" should "return completed states" in {
    val stage1 = mock[Stage[Double, Int, String]]
    val stage2 = mock[Stage[String, Float, String]]
    val onDone = mock[OnDone[Double, Int, String]]
    val f = mock[Stage[Double, Int, String] => Stage[String, Float, String]]
    val completed = onDone.complete(f)

    (f.apply _).expects(stage1).returns(stage2)
    (onDone.onComplete _).expects().returns(State.Success(stage1))
    completed.onSuccess() shouldBe State.Complete(stage2)

    (f.apply _).expects(stage1).returns(stage2)
    (onDone.onComplete _).expects().returns(State.Complete(stage1))
    completed.onComplete() shouldBe State.Complete(stage2)

    (f.apply _).expects(stage1).returns(stage2)
    (onDone.onError _).expects().returns(State.Error(stage1, "error"))
    completed.onError() shouldBe State.Error(stage2, "error")

    val panic = State.Panic(new Exception)
    (onDone.onPanic _).expects().returns(panic)
    completed.onPanic() shouldBe panic

    (onDone.dispose _).expects()
    completed.dispose()
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
      (onDone2.onComplete _).expects().returns(State.Complete(stage2))
      (onDone1.onComplete _).expects().returns(State.Success(stage1))
    }
    inside((onDone1 <~ onDone2).onComplete()) { case State.Complete(stage) =>
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some(42, mock[OnDone[Unit, Int, Nothing]]))
        (stage2.apply _).expects(42).returns(Yield.Some("xi", mock[OnDone[Int, String, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some("xi", _) => }
    }
  }

  it should "return a correct state with onError" in {
    val onDone1 = mock[OnDone.Safe[Unit, Int, String]]
    val stage1 = mock[Stage[Unit, Int, String]]
    val onDone2 = mock[OnDone.Safe[Int, String, String]]
    val stage2 = mock[Stage[Int, String, String]]
    inSequence {
      (onDone2.onError _).expects().returns(State.Error(stage2, "error"))
      (onDone1.onError _).expects().returns(State.Success(stage1))
    }
    inside((onDone1 <~ onDone2).onError()) { case State.Error(stage, errors) =>
      errors shouldBe NonEmptyChain("error")
      inSequence {
        (stage1.apply _).expects(()).returns(Yield.Some(42, mock[OnDone[Unit, Int, Nothing]]))
        (stage2.apply _).expects(42).returns(Yield.Some("xi", mock[OnDone[Int, String, Nothing]]))
      }
      stage(()) should matchPattern { case Yield.Some("xi", _) => }
    }
  }

  it should "return a correct state with onPanic" in {
    val onDone1 = mock[OnDone.Safe[Unit, Int, String]]
    val onDone2 = mock[OnDone.Safe[Int, String, String]]
    val expectedException = new Exception("panic")
    inSequence {
      (onDone2.onPanic _).expects().returns(State.Panic(expectedException))
      (onDone1.onPanic _).expects().returns(State.Error(mock[Stage[Unit, Int, String]], "error"))
    }
    (onDone1 <~ onDone2).onPanic() shouldBe State.Panic(expectedException)
  }

  it should "correctly compose dispose" in {
    val onDone1 = mock[OnDone.Safe[Unit, Int, Nothing]]
    val onDone2 = mock[OnDone.Safe[Int, String, Nothing]]
    inSequence {
      (onDone2.dispose _).expects()
      (onDone1.dispose _).expects()
    }
    (onDone1 <~ onDone2).dispose()
  }
}
