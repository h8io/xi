package h8io.xi.stages.util

import h8io.xi.stages.{State, Yield}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ConstTest extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks {
  "Const" should "always return the same value" in
    forAll { (out: String) =>
      val stage = Const(out)
      inside(stage(mock[AnyRef])) { case Yield.Some(`out`, State.Success, onDone) =>
        onDone.onSuccess() shouldBe stage
        onDone.onComplete() shouldBe stage
        onDone.onError() shouldBe stage
      }
    }
}
