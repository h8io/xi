package h8io.xi.stages.std

import h8io.xi.stages.{State, Yield}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeadEndTest extends AnyFlatSpec with Matchers with Inside {
  "DeadEnd" should "always return Yield.None and State.Complete, referred to itself" in
    inside(DeadEnd("xi")) { case Yield.None(onDone) =>
      val expectedState = State.Complete(DeadEnd)
      onDone.onSuccess() shouldBe expectedState
      onDone.onComplete() shouldBe expectedState
      onDone.onError() shouldBe expectedState
      onDone.onPanic() shouldBe expectedState
      onDone.dispose()
    }
}
