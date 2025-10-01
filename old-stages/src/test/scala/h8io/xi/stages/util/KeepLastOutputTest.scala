package h8io.xi.stages.util

import cats.data.NonEmptyChain
import h8io.xi.stages.{OnDone, Stage, State, Yield}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class KeepLastOutputTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Initial stage" should "should be None" in {
    val stage = mock[Stage[Long, Int, Nothing]]
    KeepLastOutput(stage) shouldBe KeepLastOutput.None(stage)
  }

  "KeepLastOutput.None" should "stay None if the underlying stage returns Yield.None" in {
    val stage = mock[Stage[Long, Int, String]]
    val onDone = mock[OnDone[Long, Int, String]]
    val in = Random.nextLong()
    (stage.apply _).expects(in).returns(Yield.None(onDone))
    inside(KeepLastOutput.None(stage)(in)) { case Yield.None(kloOnDone) =>
      val updatedStage = mock[Stage[Long, Int, Nothing]]
      val kloUpdatedStage = KeepLastOutput.None(updatedStage)
      testOnDone(onDone, updatedStage, kloOnDone, kloUpdatedStage)
    }
  }

  it should "become Some if the underlying stage returns Yield.Some" in {
    val stage = mock[Stage[Long, Int, String]]
    val onDone = mock[OnDone[Long, Int, String]]
    val in = Random.nextLong()
    val out = Random.nextInt()
    (stage.apply _).expects(in).returns(Yield.Some(out, onDone))
    inside(KeepLastOutput.None(stage)(in)) { case Yield.Some(out, kloOnDone) =>
      val updatedStage = mock[Stage[Long, Int, Nothing]]
      val kloUpdatedStage = KeepLastOutput.Some(out, updatedStage)
      testOnDone(onDone, updatedStage, kloOnDone, kloUpdatedStage)
    }
  }

  "KeepLastOutput.Some" should "keep the old output if the underlying stage returns Yield.None" in {
    val stage = mock[Stage[Long, Int, String]]
    val onDone = mock[OnDone[Long, Int, String]]
    val value = Random.nextInt()
    val in = Random.nextLong()
    (stage.apply _).expects(in).returns(Yield.None(onDone))
    inside(KeepLastOutput.Some(value, stage)(in)) { case Yield.Some(`value`, kloOnDone) =>
      val updatedStage = mock[Stage[Long, Int, Nothing]]
      val kloUpdatedStage = KeepLastOutput.Some(value, updatedStage)
      testOnDone(onDone, updatedStage, kloOnDone, kloUpdatedStage)
    }
  }

  it should "use the new output if the underlying stage returns Yield.Some" in {
    val stage = mock[Stage[Long, Int, String]]
    val onDone = mock[OnDone[Long, Int, String]]
    val value = Random.nextInt()
    val in = Random.nextLong()
    val out = value + 42
    (stage.apply _).expects(in).returns(Yield.Some(out, onDone))
    inside(KeepLastOutput.Some(value, stage)(in)) { case Yield.Some(`out`, kloOnDone) =>
      val updatedStage = mock[Stage[Long, Int, Nothing]]
      val kloUpdatedStage = KeepLastOutput.Some(out, updatedStage)
      testOnDone(onDone, updatedStage, kloOnDone, kloUpdatedStage)
    }
  }

  private def testOnDone(onDone: OnDone[Long, Int, String], updatedStage: Stage[Long, Int, String],
      kloOnDone: OnDone[Long, Int, String], kloUpdatedStage: Stage[Long, Int, String]): Unit = {
    (onDone.onSuccess _).expects().returns(State.Success(updatedStage))
    kloOnDone.onSuccess() shouldBe State.Success(kloUpdatedStage)

    (onDone.onComplete _).expects().returns(State.Complete(updatedStage))
    kloOnDone.onComplete() shouldBe State.Complete(kloUpdatedStage)

    (onDone.onError _).expects().returns(State.Error(updatedStage, "error"))
    inside(kloOnDone.onError()) { case State.Error(`kloUpdatedStage`, errors) =>
      errors shouldBe NonEmptyChain.one("error")
    }

    (onDone.onPanic _).expects().returns(State.Complete(updatedStage))
    kloOnDone.onPanic() shouldBe State.Complete(kloUpdatedStage)

    (onDone.dispose _).expects()
    kloOnDone.dispose()
  }
}
