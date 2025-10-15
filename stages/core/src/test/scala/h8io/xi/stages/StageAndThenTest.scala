package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StageAndThenTest
    extends AnyFlatSpec
    with Matchers
    with Inside
    with MockFactory
    with ScalaCheckPropertyChecks
    with CoreStagesArbitraries {
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
          signal shouldBe previousSignal ~> nextSignal
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
        signal shouldBe previousSignal ~> nextSignal
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
      case Signal.Error(_) => (onDone.onError _).expects().returns(stage)
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
