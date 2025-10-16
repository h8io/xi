package h8io.xi.stages.wrappers

import h8io.xi.stages.Stage
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IdentityTest extends AnyFlatSpec with Matchers with MockFactory {
  "Identity" should "transform stage to itself" in {
    val stage = mock[Stage[Array[Int], String, Exception]]
    Identity[Array[Int], String, Exception](stage) shouldBe stage
  }
}
