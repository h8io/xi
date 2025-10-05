package h8io.xi.stages

import h8io.xi.stages.Stage.{AndThen, Decorator}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.sql.Timestamp
import java.time.Instant

class StageTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {
  "~>" should "produce AndThen object" in {
    val previous = mock[Stage[String, Long, Nothing]]
    val next = mock[Stage[Long, Timestamp, String]]
    previous ~> next shouldBe AndThen(previous, next)
  }

  "<~" should "produce AndThen object" in {
    val previous = mock[Stage[Instant, Int, String]]
    val next = mock[Stage[Int, String, Nothing]]
    next <~ previous shouldBe AndThen(previous, next)
  }

  "dispose" should "do nothing" in {
    noException should be thrownBy new Stage[Instant, Timestamp, String] {
      def apply(in: Instant): Yield[Instant, Timestamp, String] = throw new NotImplementedError
    }.dispose()
  }

  "AndThen" should "call sequentially stages and return the correct Yield for Some ~> Some" in
    forAll { (previousState: State[String], nextState: State[String], in: Int, previousOut: String, nextOut: Long) =>
      val previousStage = mock[Stage[Int, String, String]]
      val previousOnDone = mock[OnDone[Int, String, String]]
      val nextStage = mock[Stage[String, Long, String]]
      val nextOnDone = mock[OnDone[String, Long, String]]
      inSequence {
        (previousStage.apply _).expects(in).returns(Yield.Some(previousOut, previousState, previousOnDone))
        (nextStage.apply _).expects(previousOut).returns(Yield.Some(nextOut, nextState, nextOnDone))
      }
      inside(AndThen(previousStage, nextStage)(in)) { case Yield.Some(`nextOut`, state, onDone) =>
        state shouldBe previousState ~> nextState
        val updatedPreviousStage = mock[Stage[Int, String, String]]
        val updatedNextStage = mock[Stage[String, Long, Nothing]]
        inSequence {
          armOnDone(nextOnDone, state, updatedNextStage)
          armOnDone(previousOnDone, state, updatedPreviousStage)
        }
        state(onDone) shouldBe AndThen(updatedPreviousStage, updatedNextStage)
      }
    }

  it should "call sequentially stages and return the correct Yield for Some ~> None" in
    forAll { (previousState: State[String], nextState: State[String], in: Int, out: String) =>
      val previousStage = mock[Stage[Int, String, String]]
      val previousOnDone = mock[OnDone[Int, String, String]]
      val nextStage = mock[Stage[String, Long, String]]
      val nextOnDone = mock[OnDone[String, Long, String]]
      inSequence {
        (previousStage.apply _).expects(in).returns(Yield.Some(out, previousState, previousOnDone))
        (nextStage.apply _).expects(out).returns(Yield.None(nextState, nextOnDone))
      }
      inside(AndThen(previousStage, nextStage)(in)) { case Yield.None(state, onDone) =>
        state shouldBe previousState ~> nextState
        val updatedPreviousStage = mock[Stage[Int, String, String]]
        val updatedNextStage = mock[Stage[String, Long, Nothing]]
        inSequence {
          armOnDone(nextOnDone, state, updatedNextStage)
          armOnDone(previousOnDone, state, updatedPreviousStage)
        }
        state(onDone) shouldBe AndThen(updatedPreviousStage, updatedNextStage)
      }
    }

  it should "call the first stage only and return the correct Yield for None ~> any Yield" in
    forAll { (previousState: State[String], in: Int) =>
      val previousStage = mock[Stage[Int, String, String]]
      val previousOnDone = mock[OnDone[Int, String, String]]
      val nextStage = mock[Stage[String, Long, String]]
      (previousStage.apply _).expects(in).returns(Yield.None(previousState, previousOnDone))
      inside(AndThen(previousStage, nextStage)(in)) { case Yield.None(`previousState`, onDone) =>
        val updatedPreviousStage = mock[Stage[Int, String, String]]
        armOnDone(previousOnDone, previousState, updatedPreviousStage)
        previousState(onDone) shouldBe AndThen(updatedPreviousStage, nextStage)
      }
    }

  private def armOnDone[I, O, E](
      onDone: OnDone[I, O, E],
      state: State[E],
      stage: Stage[I, O, E]): (OnDone[I, O, E], Stage[I, O, E]) = {
    state match {
      case State.Success => (onDone.onSuccess _).expects().returns(stage)
      case State.Complete => (onDone.onComplete _).expects().returns(stage)
      case State.Error(_) => (onDone.onError _).expects().returns(stage)
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
    AndThen(previousStage, nextStage).dispose()
  }

  "Decorator's dispose" should "call underlying stage's dispose method" in {
    val underlying = mock[Stage[Any, Nothing, Nothing]]
    (underlying.dispose _).expects()
    noException should be thrownBy new Decorator[Any, Nothing, Nothing] {
      val stage: Stage[Any, Nothing, Nothing] = underlying
      def apply(in: Any): Yield[Any, Nothing, Nothing] = throw new NoSuchMethodError
    }.dispose()
  }
}
