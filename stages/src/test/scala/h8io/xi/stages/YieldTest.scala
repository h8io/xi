package h8io.xi.stages

import org.scalacheck.Arbitrary
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Instant

class YieldTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {
  private implicit def genYieldSome[I, O: Arbitrary, E: Arbitrary]: Arbitrary[Yield.Some[I, O, E]] =
    Arbitrary {
      for {
        out <- Arbitrary.arbitrary[O]
        state <- genState[E].arbitrary
      } yield Yield.Some(out, state, mock[OnDone[I, O, E]])
    }

  private implicit def genYieldNone[I, O, E: Arbitrary]: Arbitrary[Yield.None[I, O, E]] =
    Arbitrary(genState[E].arbitrary.map(state => Yield.None(state, mock[OnDone[I, O, E]])))

  "~>" should "combine Some and Some correctly" in
    forAll { (previousYield: Yield.Some[Long, Instant, String], nextYield: Yield.Some[Instant, String, String]) =>
      inside(previousYield ~> nextYield) {
        case Yield.Some(nextYield.out, state, onDone) =>
          state shouldBe previousYield.state ~> nextYield.state
          val previousStage = mock[Stage[Long, Instant, String]]
          val nextStage = mock[Stage[Instant, String, String]]
          val stage = previousStage ~> nextStage

          inSequence {
            (nextYield.onDone.onSuccess _).expects().returns(nextStage)
            (previousYield.onDone.onSuccess _).expects().returns(previousStage)
          }
          onDone.onSuccess() shouldBe stage

          inSequence {
            (nextYield.onDone.onComplete _).expects().returns(nextStage)
            (previousYield.onDone.onComplete _).expects().returns(previousStage)
          }
          onDone.onComplete() shouldBe stage

          inSequence {
            (nextYield.onDone.onError _).expects().returns(nextStage)
            (previousYield.onDone.onError _).expects().returns(previousStage)
          }
          onDone.onError() shouldBe stage
      }
    }

  it should "combine Some and None correctly" in
    forAll { (previousYield: Yield.Some[Long, Instant, String], nextYield: Yield.None[Instant, String, String]) =>
      inside(previousYield ~> nextYield) {
        case Yield.None(state, onDone) =>
          state shouldBe previousYield.state ~> nextYield.state
          val previousStage = mock[Stage[Long, Instant, String]]
          val nextStage = mock[Stage[Instant, String, String]]
          val stage = previousStage ~> nextStage

          inSequence {
            (nextYield.onDone.onSuccess _).expects().returns(nextStage)
            (previousYield.onDone.onSuccess _).expects().returns(previousStage)
          }
          onDone.onSuccess() shouldBe stage

          inSequence {
            (nextYield.onDone.onComplete _).expects().returns(nextStage)
            (previousYield.onDone.onComplete _).expects().returns(previousStage)
          }
          onDone.onComplete() shouldBe stage

          inSequence {
            (nextYield.onDone.onError _).expects().returns(nextStage)
            (previousYield.onDone.onError _).expects().returns(previousStage)
          }
          onDone.onError() shouldBe stage
      }
    }

  it should "combine None and stage correctly" in
    forAll { (previousYield: Yield.None[Long, Instant, String]) =>
      val nextStage = mock[Stage[Instant, String, String]]
      inside(previousYield ~> nextStage) {
        case Yield.None(previousYield.state, onDone) =>
          val previousStage = mock[Stage[Long, Instant, String]]
          val stage = previousStage ~> nextStage

          (previousYield.onDone.onSuccess _).expects().returns(previousStage)
          onDone.onSuccess() shouldBe stage

          (previousYield.onDone.onComplete _).expects().returns(previousStage)
          onDone.onComplete() shouldBe stage

          (previousYield.onDone.onError _).expects().returns(previousStage)
          onDone.onError() shouldBe stage
      }
    }
}
