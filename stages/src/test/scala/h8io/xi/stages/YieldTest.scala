package h8io.xi.stages

import org.scalacheck.Arbitrary
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.*
import java.time.format.DateTimeFormatter

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

  "map" should "transform Some content" in
    forAll { (initialOut: Array[Byte], initialState: State[Instant], mappedOut: Long, mappedState: State[String]) =>
      val initialOnDone = mock[OnDone[String, Array[Byte], Instant]]("initial OnDone")
      val mappedOnDone = mock[OnDone[Array[Byte], Long, String]]("mapped OnDone")
      val mapOut = mock[Array[Byte] => Long]("mapOut")
      (mapOut.apply _).expects(initialOut).returns(mappedOut)
      val mapState = mock[State[Instant] => State[String]]("mapState")
      (mapState.apply _).expects(initialState).returns(mappedState)
      val mapOnDone = mock[OnDone[String, Array[Byte], Instant] => OnDone[Array[Byte], Long, String]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.Some(initialOut, initialState, initialOnDone).map(mapOut, mapState, mapOnDone) shouldBe
        Yield.Some(mappedOut, mappedState, mappedOnDone)
    }

  it should "transform None content" in
    forAll { (initialState: State[Long], mappedState: State[Exception]) =>
      val initialOnDone = mock[OnDone[ZonedDateTime, DateTimeFormatter, Long]]("initial OnDone")
      val mappedOnDone = mock[OnDone[Duration, OffsetDateTime, String]]("mapped OnDone")
      val mapOut = mock[DateTimeFormatter => OffsetDateTime]("mapOut")
      val mapState = mock[State[Long] => State[Exception]]("mapState")
      (mapState.apply _).expects(initialState).returns(mappedState)
      val mapOnDone =
        mock[OnDone[ZonedDateTime, DateTimeFormatter, Long] => OnDone[Duration, OffsetDateTime, String]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.None(initialState, initialOnDone).map(mapOut, mapState, mapOnDone) shouldBe
        Yield.None(mappedState, mappedOnDone)
    }

  "mapOnDone" should "transform Some content" in
    forAll { (out: LocalDateTime, initialState: State[Long], mappedState: State[Exception]) =>
      val initialOnDone = mock[OnDone[Long, LocalDateTime, Long]]("initial OnDone")
      val mappedOnDone = mock[OnDone[String, LocalDateTime, Exception]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[Long, LocalDateTime, Long] => OnDone[String, LocalDateTime, Exception]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.Some(out, initialState, initialOnDone).mapOnDone(mappedState, mapOnDone) shouldBe
        Yield.Some(out, mappedState, mappedOnDone)
    }

  it should "transform None content" in
    forAll { (initialState: State[Exception], mappedState: State[String]) =>
      val initialOnDone = mock[OnDone[ZonedDateTime, OffsetDateTime, Exception]]("initial OnDone")
      val mappedOnDone = mock[OnDone[Duration, OffsetDateTime, String]]("mapped OnDone")
      val mapOnDone =
        mock[OnDone[ZonedDateTime, OffsetDateTime, Exception] => OnDone[Duration, OffsetDateTime, String]]("mapOnDone")
      (mapOnDone.apply _).expects(initialOnDone).returns(mappedOnDone)
      Yield.None(initialState, initialOnDone).mapOnDone[Duration, OffsetDateTime, String](
        mappedState, mapOnDone) shouldBe Yield.None(mappedState, mappedOnDone)
    }
}
