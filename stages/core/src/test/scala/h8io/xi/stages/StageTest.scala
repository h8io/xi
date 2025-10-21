package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.sql.Timestamp
import java.time.*
import java.util.UUID

class StageTest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with MockFactory
    with ScalaCheckPropertyChecks
    with StagesCoreArbitraries {
  "dispose" should "do nothing" in {
    noException should be thrownBy new Stage[Instant, Timestamp, String] {
      def apply(in: Instant): Yield[Instant, Timestamp, String] = throw new NotImplementedError
    }.dispose()
  }

  "execute" should "run onDone and return Outcome.Some" in
    forAll { (in: Long, out: String, signal: Signal[UUID]) =>
      val stage = mock[Stage[Long, String, UUID]]
      val onDone = mock[OnDone[Long, String, UUID]]
      val evolved = mock[Stage[Long, String, UUID]]
      inSequence {
        (stage.apply _).expects(in).returns(Yield.Some(out, signal, onDone))
        signal match {
          case Signal.Success => (onDone.onSuccess _).expects().returns(evolved)
          case Signal.Complete => (onDone.onComplete _).expects().returns(evolved)
          case _: Signal.Error[UUID] => (onDone.onError _).expects().returns(evolved)
        }
      }
      inside(stage.execute(in)) { case Outcome.Some(`out`, `signal`, dispose) =>
        (evolved.dispose _).expects()
        dispose()
      }
    }

  it should "run onDone and return Outcome.None" in
    forAll { (in: Instant, signal: Signal[Long]) =>
      val stage = mock[Stage[Instant, Boolean, Long]]
      val onDone = mock[OnDone[Instant, Boolean, Long]]
      val evolved = mock[Stage[Instant, Boolean, Long]]
      inSequence {
        (stage.apply _).expects(in).returns(Yield.None(signal, onDone))
        signal match {
          case Signal.Success => (onDone.onSuccess _).expects().returns(evolved)
          case Signal.Complete => (onDone.onComplete _).expects().returns(evolved)
          case _: Signal.Error[Long] => (onDone.onError _).expects().returns(evolved)
        }
      }
      inside(stage.execute(in)) { case Outcome.None(`signal`, dispose) =>
        (evolved.dispose _).expects()
        dispose()
      }
    }

  "~>" should "produce Stage.AndThen object with a stage argument" in {
    val previous = mock[Stage[String, Long, Nothing]]
    val next = mock[Stage[Long, Timestamp, String]]
    previous ~> next shouldBe Stage.AndThen(previous, next)
  }

  it should "produce a alterator with a alterator argument" in {
    val stage = mock[Stage[Int, Long, UUID]]
    val alterator = mock[Alterator[Stage[ZoneId, ZonedDateTime, String], Stage[Long, String, Nothing]]]
    val in = mock[Stage[ZoneId, ZonedDateTime, String]]
    val out = mock[Stage[Long, String, Nothing]]
    (alterator.apply _).expects(in).returns(out)
    val result: Stage[Int, String, UUID] = (stage ~> alterator)(in)
    result shouldBe stage ~> out
    (alterator.apply _).expects(in).returns(out)
    stage ~> alterator <| in shouldBe stage ~> out
  }

  "<~" should "produce Stage.AndThen object" in {
    val previous = mock[Stage[Instant, Int, String]]
    val next = mock[Stage[Int, String, Nothing]]
    next <~ previous shouldBe Stage.AndThen(previous, next)
  }

  "|>" should "apply alterator to stage" in {
    val stage = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val alterator = mock[Alterator[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val out = mock[Stage[UUID, Instant, Long]]
    (alterator.apply _).expects(stage).returns(out)
    stage |> alterator shouldBe out
  }

  "alterator" should "be a leftAlterator" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    left.alterator(right) shouldBe left ~> right
  }

  "leftAlterator" should "produce a composition with predefined left operand as an be alterator" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    val alterator: Alterator[Stage[Long, Duration, Exception], Stage[Int, Duration, Exception]] = left.leftAlterator
    alterator(right) shouldBe left ~> right
  }

  "rightAlterator" should "produce a composition with predefined right operand as an be alterator" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    val alterator: Alterator[Stage[Int, Long, Nothing], Stage[Int, Duration, Exception]] = right.rightAlterator
    alterator(left) shouldBe left ~> right
  }

  "AndThen" should "call sequentially stages and return the correct Yield for Some ~> Some" in
    forAll {
      (previousSignal: Signal[String], nextSignal: Signal[String], in: Int, previousOut: String, nextOut: Long) =>
        val previousStage = mock[Stage[Int, String, String]]
        val previousOnDone = mock[OnDone[Int, String, String]]
        val nextStage = mock[Stage[String, Long, String]]
        val nextOnDone = mock[OnDone[String, Long, String]]
        inSequence {
          (previousStage.apply _).expects(in).returns(Yield.Some(previousOut, previousSignal, previousOnDone))
          (nextStage.apply _).expects(previousOut).returns(Yield.Some(nextOut, nextSignal, nextOnDone))
        }
        inside(Stage.AndThen(previousStage, nextStage)(in)) { case Yield.Some(`nextOut`, signal, onDone) =>
          signal shouldBe previousSignal ++ nextSignal
          val updatedPreviousStage = mock[Stage[Int, String, String]]
          val updatedNextStage = mock[Stage[String, Long, Nothing]]
          inSequence {
            armOnDone(nextOnDone, signal, updatedNextStage)
            armOnDone(previousOnDone, signal, updatedPreviousStage)
          }
          signal(onDone) shouldBe Stage.AndThen(updatedPreviousStage, updatedNextStage)
        }
    }

  it should "call sequentially stages and return the correct Yield for Some ~> None" in
    forAll { (previousSignal: Signal[String], nextSignal: Signal[String], in: Int, out: String) =>
      val previousStage = mock[Stage[Int, String, String]]
      val previousOnDone = mock[OnDone[Int, String, String]]
      val nextStage = mock[Stage[String, Long, String]]
      val nextOnDone = mock[OnDone[String, Long, String]]
      inSequence {
        (previousStage.apply _).expects(in).returns(Yield.Some(out, previousSignal, previousOnDone))
        (nextStage.apply _).expects(out).returns(Yield.None(nextSignal, nextOnDone))
      }
      inside(Stage.AndThen(previousStage, nextStage)(in)) { case Yield.None(signal, onDone) =>
        signal shouldBe previousSignal ++ nextSignal
        val updatedPreviousStage = mock[Stage[Int, String, String]]
        val updatedNextStage = mock[Stage[String, Long, Nothing]]
        inSequence {
          armOnDone(nextOnDone, signal, updatedNextStage)
          armOnDone(previousOnDone, signal, updatedPreviousStage)
        }
        signal(onDone) shouldBe Stage.AndThen(updatedPreviousStage, updatedNextStage)
      }
    }

  it should "call the first stage only and return the correct Yield for None ~> any Yield" in
    forAll { (previousSignal: Signal[String], in: Int) =>
      val previousStage = mock[Stage[Int, String, String]]
      val previousOnDone = mock[OnDone[Int, String, String]]
      val nextStage = mock[Stage[String, Long, String]]
      (previousStage.apply _).expects(in).returns(Yield.None(previousSignal, previousOnDone))
      inside(Stage.AndThen(previousStage, nextStage)(in)) { case Yield.None(`previousSignal`, onDone) =>
        val updatedPreviousStage = mock[Stage[Int, String, String]]
        armOnDone(previousOnDone, previousSignal, updatedPreviousStage)
        previousSignal(onDone) shouldBe Stage.AndThen(updatedPreviousStage, nextStage)
      }
    }

  private def armOnDone[I, O, E](
      onDone: OnDone[I, O, E],
      signal: Signal[E],
      stage: Stage[I, O, E]): (OnDone[I, O, E], Stage[I, O, E]) = {
    signal match {
      case Signal.Success => (onDone.onSuccess _).expects().returns(stage)
      case Signal.Complete => (onDone.onComplete _).expects().returns(stage)
      case Signal.Error(_, _) => (onDone.onError _).expects().returns(stage)
    }
    (onDone, stage)
  }

  it should "call the method dispose in the reversed order" in {
    val previousStage = mock[Stage[Int, String, String]]
    val nextStage = mock[Stage[String, Long, String]]
    inSequence {
      (nextStage.dispose _).expects()
      (previousStage.dispose _).expects()
    }
    Stage.AndThen(previousStage, nextStage).dispose()
  }
}
