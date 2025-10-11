package h8io.xi.stages.std

import h8io.xi.stages.{Signal, Yield}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IdentityTest extends AnyFlatSpec with Matchers with Inside with ScalaCheckPropertyChecks {
  "Identity" should "return input" in {
    val identity = Identity[String]
    forAll { (in: String) =>
      inside(Identity(in)) { case Yield.Some(`in`, Signal.Success, onDone) =>
        onDone.onSuccess() shouldBe identity
        onDone.onComplete() shouldBe identity
        onDone.onError() shouldBe identity
      }
    }
  }
}
