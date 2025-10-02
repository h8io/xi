package h8io.xi.stages.util

import h8io.xi.stages.{State, Yield}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IdentityTest extends AnyFlatSpec with Matchers with Inside with ScalaCheckPropertyChecks {
  "Identity" should "return input" in {
    val identity = Identity[String]
    forAll { (in: String) =>
      inside(identity(in)) { case Yield.Some(`in`, State.Success, onDone) =>
        onDone.onSuccess() shouldBe identity
        onDone.onComplete() shouldBe identity
        onDone.onError() shouldBe identity
      }
    }
  }
}
