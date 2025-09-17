package h8io.xi

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FactorialTest extends AnyFlatSpec with Matchers {
  "n!" should "be 1 if n = 0" in {
    Factorial(0) shouldBe BigInt(1)
  }

  it should "be 1 if n = 1" in {
    Factorial(1) shouldBe BigInt(1)
  }
}
