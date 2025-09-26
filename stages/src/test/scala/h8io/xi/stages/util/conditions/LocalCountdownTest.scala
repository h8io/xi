package h8io.xi.stages.util.conditions

import h8io.xi.stages.Condition
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LocalCountdownTest extends AnyFlatSpec with Matchers with MockFactory {
  "apply" should "return Condition.False if n is not positive" in {
    LocalCountdown(0) shouldBe Condition.False
    LocalCountdown(-Long.MaxValue) shouldBe Condition.False
    LocalCountdown(Long.MinValue) shouldBe Condition.False
  }

  it should "return LocalCountdown.Impl if n is positive" in {
    LocalCountdown(1) shouldBe LocalCountdown.Impl(1, 1)
    LocalCountdown(Long.MaxValue) shouldBe LocalCountdown.Impl(Long.MaxValue, Long.MaxValue)
  }

  "Impl" should "return true on check if i > 0" in { LocalCountdown.Impl(1, 7).check shouldBe true }

  it should "return false on check if i == 0" in { LocalCountdown.Impl(0, 3).check shouldBe false }

  it should "decrement i on advance" in { LocalCountdown.Impl(3, 17).advance() shouldBe LocalCountdown.Impl(2, 17) }

  it should "set i to n on reset" in { LocalCountdown.Impl(5, 42).reset() shouldBe LocalCountdown.Impl(42, 42) }
}
