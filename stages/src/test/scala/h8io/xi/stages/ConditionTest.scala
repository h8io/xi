package h8io.xi.stages

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConditionTest extends AnyFlatSpec with Matchers {
  "Condition.True" should "always be true" in { Condition.True.check shouldBe true }
  it should "be advanced to itself" in { Condition.True.advance shouldBe Condition.True }
  it should "be reset to itself" in { Condition.True.reset shouldBe Condition.True }

  "Condition.False" should "always be true" in { Condition.False.check shouldBe false }
  it should "be advanced to itself" in { Condition.False.advance shouldBe Condition.False }
  it should "be reset to itself" in { Condition.False.reset shouldBe Condition.False }
}
