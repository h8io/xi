package h8io.xi.stages.examples

import h8io.xi.stages.Yield
import org.scalacheck.Gen
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FactorialTest extends AnyFlatSpec with Matchers with Inside with ScalaCheckPropertyChecks {
  def factorial(n: Int): BigInt = (1 to n foldLeft BigInt(1))(_ * _)

  "Factorial1" should "calculate factorial for positive arguments" in
    forAll(Gen.choose(1, 1000)) { n =>
      inside(Factorial1.stage(n)(())) { case Yield.Some(out, _, _) => out shouldBe factorial(n) }
    }

  it should "not calculate factorial for zero" in {
    Factorial1.stage(0)(()) should matchPattern { case Yield.None(_, _) => }
  }

  it should "not calculate factorial for negative arguments" in
    forAll(Gen.choose(Int.MinValue, -1)) { n =>
      Factorial1.stage(n)(()) should matchPattern { case Yield.None(_, _) => }
    }
}
