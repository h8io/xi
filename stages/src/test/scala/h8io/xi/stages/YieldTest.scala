package h8io.xi.stages

import org.scalacheck.Arbitrary
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.*

class YieldTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {
  private implicit def genYieldSome[I, O: Arbitrary, E: Arbitrary]: Arbitrary[Yield.Some[I, O, E]] =
    Arbitrary {
      for {
        out <- Arbitrary.arbitrary[O]
        signal <- genSignal[E].arbitrary
      } yield Yield.Some(out, signal, mock[OnDone[I, O, E]])
    }

  private implicit def genYieldNone[I, O, E: Arbitrary]: Arbitrary[Yield.None[I, O, E]] =
    Arbitrary(genSignal[E].arbitrary.map(signal => Yield.None(signal, mock[OnDone[I, O, E]])))

  "~>" should "combine Some and Some correctly" in
    forAll { (previousYield: Yield.Some[Long, Instant, String], nextYield: Yield.Some[Instant, String, String]) =>
      inside(previousYield ~> nextYield) {
        case Yield.Some(nextYield.out, signal, onDone) =>
          signal shouldBe previousYield.signal ~> nextYield.signal
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
        case Yield.None(signal, onDone) =>
          signal shouldBe previousYield.signal ~> nextYield.signal
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
        case Yield.None(previousYield.`signal`, onDone) =>
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

  "mapOnDone" should "transform Some content (signal and onDone)" in
    forAll { (out: LocalDateTime, initialSignal: Signal[Long], mappedSignal: Signal[Exception]) =>
      val initialOnDone = mock[OnDone[Long, LocalDateTime, Long]]("initial OnDone")
      val mappedOnDone = mock[OnDone[String, LocalDateTime, Exception]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[Long, LocalDateTime, Long] => OnDone[String, LocalDateTime, Exception]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.Some(out, initialSignal, initialOnDone).mapOnDone(mappedSignal, mapOnDone) shouldBe
        Yield.Some(out, mappedSignal, mappedOnDone)
    }

  it should "transform Some content (onDone)" in
    forAll { (out: LocalDateTime, signal: Signal[Long]) =>
      val initialOnDone = mock[OnDone[Long, LocalDateTime, Long]]("initial OnDone")
      val mappedOnDone = mock[OnDone[String, LocalDateTime, Long]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[Long, LocalDateTime, Long] => OnDone[String, LocalDateTime, Long]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.Some(out, signal, initialOnDone).mapOnDone(mapOnDone) shouldBe Yield.Some(out, signal, mappedOnDone)
    }

  it should "transform None content (signal and onDone)" in
    forAll { (initialSignal: Signal[Exception], mappedSignal: Signal[String]) =>
      val initialOnDone = mock[OnDone[ZonedDateTime, OffsetDateTime, Exception]]("initial OnDone")
      val mappedOnDone = mock[OnDone[Duration, OffsetDateTime, String]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[ZonedDateTime, OffsetDateTime, Exception] => OnDone[Duration, OffsetDateTime, String]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.None(initialSignal, initialOnDone).mapOnDone(mappedSignal, mapOnDone) shouldBe
        Yield.None(mappedSignal, mappedOnDone)
    }

  it should "transform None content (onDone)" in
    forAll { (signal: Signal[Int]) =>
      val initialOnDone = mock[OnDone[ZonedDateTime, OffsetDateTime, Int]]("initial OnDone")
      val mappedOnDone = mock[OnDone[Duration, OffsetDateTime, Int]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[ZonedDateTime, OffsetDateTime, Int] => OnDone[Duration, OffsetDateTime, Int]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.None(signal, initialOnDone).mapOnDone(mapOnDone) shouldBe Yield.None(signal, mappedOnDone)
    }
}
