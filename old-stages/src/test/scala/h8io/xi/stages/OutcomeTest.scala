package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OutcomeTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "toYield" should "create a correct Yield.Some object" in {
    val dispose = mock[() => Unit]
    val state = State.Panic(new Exception)
    inside(Outcome.Some(42, State.Complete(mock[Stage[Unit, Int, Nothing]]), dispose).toYield(state)) {
      case Yield.Some(42, onDone) =>
        onDone.onSuccess() shouldBe state
        onDone.onComplete() shouldBe state
        onDone.onError() shouldBe state
        onDone.onPanic() shouldBe state
        (dispose.apply _).expects()
        onDone.dispose()
    }
  }

  it should "create a correct Yield.None object" in {
    val dispose = mock[() => Unit]
    val state = State.Panic(new Exception)
    inside(Outcome.None(State.Complete(mock[Stage[Unit, Int, Nothing]]), dispose).toYield(state)) {
      case Yield.None(onDone) =>
        onDone.onSuccess() shouldBe state
        onDone.onComplete() shouldBe state
        onDone.onError() shouldBe state
        onDone.onPanic() shouldBe state
        (dispose.apply _).expects()
        onDone.dispose()
    }
  }
}
