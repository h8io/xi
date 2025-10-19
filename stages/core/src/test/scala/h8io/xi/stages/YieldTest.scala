package h8io.xi.stages

import cats.implicits.catsSyntaxSemigroup
import h8io.xi.stages.test.signalMonoid
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.*

class YieldTest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with MockFactory
    with ScalaCheckPropertyChecks
    with CoreStagesArbitraries {
  "compose method" should "compose Some and Some correctly" in
    forAll {
      (previousYieldSupplier: OnDoneToYieldSome[Long, Instant, String],
          nextYieldSupplier: OnDoneToYieldSome[Instant, String, String]) =>
        val previousOnDone = mock[OnDone[Long, Instant, String]]
        val previousYield = previousYieldSupplier(previousOnDone)
        val nextOnDone = mock[OnDone[Instant, String, String]]
        val nextYield = nextYieldSupplier(nextOnDone)
        inside(previousYield.compose(nextYield)) {
          case Yield.Some(nextYield.out, signal, onDone) =>
            signal shouldBe previousYield.signal |+| nextYield.signal
            val previousStage = mock[Stage[Long, Instant, String]]
            val nextStage = mock[Stage[Instant, String, String]]
            val stage = previousStage ~> nextStage

            inSequence {
              (nextOnDone.onSuccess _).expects().returns(nextStage)
              (previousOnDone.onSuccess _).expects().returns(previousStage)
            }
            onDone.onSuccess() shouldBe stage

            inSequence {
              (nextOnDone.onComplete _).expects().returns(nextStage)
              (previousOnDone.onComplete _).expects().returns(previousStage)
            }
            onDone.onComplete() shouldBe stage

            inSequence {
              (nextOnDone.onError _).expects().returns(nextStage)
              (previousOnDone.onError _).expects().returns(previousStage)
            }
            onDone.onError() shouldBe stage
        }
    }

  it should "compose Some and None correctly" in
    forAll {
      (previousYieldSupplier: OnDoneToYieldSome[Long, Instant, String],
          nextYieldSupplier: OnDoneToYieldNone[Instant, String, String]) =>
        val previousOnDone = mock[OnDone[Long, Instant, String]]
        val previousYield = previousYieldSupplier(previousOnDone)
        val nextOnDone = mock[OnDone[Instant, String, String]]
        val nextYield = nextYieldSupplier(nextOnDone)
        inside(previousYield.compose(nextYield)) {
          case Yield.None(signal, onDone) =>
            signal shouldBe previousYield.signal |+| nextYield.signal
            val previousStage = mock[Stage[Long, Instant, String]]
            val nextStage = mock[Stage[Instant, String, String]]
            val stage = previousStage ~> nextStage

            inSequence {
              (nextOnDone.onSuccess _).expects().returns(nextStage)
              (previousOnDone.onSuccess _).expects().returns(previousStage)
            }
            onDone.onSuccess() shouldBe stage

            inSequence {
              (nextOnDone.onComplete _).expects().returns(nextStage)
              (previousOnDone.onComplete _).expects().returns(previousStage)
            }
            onDone.onComplete() shouldBe stage

            inSequence {
              (nextOnDone.onError _).expects().returns(nextStage)
              (previousOnDone.onError _).expects().returns(previousStage)
            }
            onDone.onError() shouldBe stage
        }
    }

  it should "compose None and stage correctly" in
    forAll { (previousYieldSupplier: OnDoneToYieldNone[Long, Instant, String]) =>
      val previousOnDone = mock[OnDone[Long, Instant, String]]
      val previousYield = previousYieldSupplier(previousOnDone)
      val nextStage = mock[Stage[Instant, String, String]]
      inside(previousYield.compose(nextStage)) {
        case Yield.None(previousYield.`signal`, onDone) =>
          val previousStage = mock[Stage[Long, Instant, String]]
          val stage = previousStage ~> nextStage

          (previousOnDone.onSuccess _).expects().returns(previousStage)
          onDone.onSuccess() shouldBe stage

          (previousOnDone.onComplete _).expects().returns(previousStage)
          onDone.onComplete() shouldBe stage

          (previousOnDone.onError _).expects().returns(previousStage)
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

  "mapOnDoneAndBreak" should "transform Some content" in
    forAll { (out: LocalDateTime, signal: Signal[Long]) =>
      val initialOnDone = mock[OnDone[Long, LocalDateTime, Long]]("initial OnDone")
      val mappedOnDone = mock[OnDone[String, LocalDateTime, Long]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[Long, LocalDateTime, Long] => OnDone[String, LocalDateTime, Long]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.Some(out, signal, initialOnDone).mapOnDoneAndBreak(mapOnDone) shouldBe
        Yield.Some(out, signal.break, mappedOnDone)
    }

  it should "transform None content" in
    forAll { (signal: Signal[Int]) =>
      val initialOnDone = mock[OnDone[ZonedDateTime, OffsetDateTime, Int]]("initial OnDone")
      val mappedOnDone = mock[OnDone[Duration, OffsetDateTime, Int]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[ZonedDateTime, OffsetDateTime, Int] => OnDone[Duration, OffsetDateTime, Int]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.None(signal, initialOnDone).mapOnDoneAndBreak(mapOnDone) shouldBe Yield.None(signal.break, mappedOnDone)
    }
}
