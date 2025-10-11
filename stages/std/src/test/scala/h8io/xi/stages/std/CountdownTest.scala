package h8io.xi.stages.std

import h8io.xi.stages.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inside}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CountdownTest extends AnyFlatSpec with Matchers with Inside with ScalaCheckPropertyChecks {
  "apply" should "create Countdown object where i == n if n > 0" in
    forAll(Gen.posNum[Long])(n => Countdown(n) shouldBe Countdown(n, n))

  it should "create DeadEnd object if n <= 0" in {
    Countdown(0) shouldBe DeadEnd
    forAll(Gen.choose(Long.MinValue, -1))(n => Countdown(n) shouldBe DeadEnd)
  }

  it should "return a yield with signal Complete or Error when i == 1" in
    forAll(Gen.zip(Gen.choose(1, Long.MaxValue), Arbitrary.arbitrary[Short])) { case (n, in) =>
      Countdown[Short](1, n)(in) shouldBe Yield.Some(in, Signal.Complete, OnDone.FromStage(Countdown[Short](n, n)))
    }

  it should "return yield with the same signal if i > 1" in
    forAll(Gen.zip(Gen.choose(2, Long.MaxValue), Arbitrary.arbitrary[String])) { case (n, in) =>
      def test(i: Long): Assertion =
        inside(Countdown(i, n)(in)) { case Yield.Some(`in`, Signal.Success, onDone) =>
          onDone.onSuccess() shouldBe Countdown(i - 1, n)
          onDone.onComplete() shouldBe Countdown(n, n)
          onDone.onError() shouldBe Countdown(n, n)
        }
      test(2)
      test(n)
      forAll(Gen.choose(2, n))(i => test(i))
    }
}
