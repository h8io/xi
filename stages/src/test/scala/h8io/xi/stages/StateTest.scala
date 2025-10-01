package h8io.xi.stages

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StateTest extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks with Generators {
  "Success" should "be idempotent" in {
    State.Success ~> State.Success shouldBe State.Success
  }

  "Complete" should "be idempotent" in {
    State.Complete ~> State.Complete shouldBe State.Complete
  }

  "Error" should "keep the order of causes in composition" in
    forAll { (previous: State.Error[String], next: State.Error[String]) =>
      previous ~> next shouldBe State.Error(previous.causes ++ next.causes)
    }
}
