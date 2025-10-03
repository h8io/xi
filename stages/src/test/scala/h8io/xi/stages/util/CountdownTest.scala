package h8io.xi.stages.util

import h8io.xi.stages.{Generators, Stage}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CountdownTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {
  "apply" should "create Impl object where i == n if n > 0" in
    forAll(Gen.choose(1L, 1000L)) { n =>
      val stage = mock[Stage[Any, Nothing, Nothing]]
      Countdown(n, stage) shouldBe Countdown.Impl(n, n, stage)
    }

  it should "create DeadEnd object if n <= 0" in
    forAll(Gen.choose(-1000L, 0L)) { n =>
      val stage = mock[Stage[Any, Nothing, Nothing]]
      inside(Countdown(n, stage)) { case DeadEnd(dispose) =>
        (stage.dispose _).expects()
        noException should be thrownBy dispose()
      }
    }

  "dispose" should "call stage's dispose" in
    forAll(Gen.choose(1L, 1000L), Gen.choose(1L, 1000L)) { (i, n) =>
      val stage = mock[Stage[Any, Nothing, Nothing]]
      (stage.dispose _).expects()
      noException should be thrownBy Countdown.Impl(i, i + n, stage).dispose()
    }
}
