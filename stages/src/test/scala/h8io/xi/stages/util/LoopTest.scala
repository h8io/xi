package h8io.xi.stages.util

import h8io.xi.stages.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID
import scala.annotation.tailrec

class LoopTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {
  "Loop" should "be executed until the state is Complete" in
    forAll(
      Gen.zip(
        Gen.listOf(Arbitrary.arbitrary[StateAndOnDoneToYieldSome[String, String, Nothing]]),
        Arbitrary.arbitrary[StateAndOnDoneToYield[String, String, Nothing]],
        Arbitrary.arbitrary[String]
      )) { case (yieldSuppliers, lastYieldSupplier, in) =>
      val initial = mock[Stage.Endo[String, Nothing]]("initial stage")
      val (lastIn, evolved) = genStage(yieldSuppliers, initial, in)
      val lastOnDone = mock[OnDone[String, String, Nothing]]("last onDone")
      val lastYield = lastYieldSupplier(State.Complete, lastOnDone)
      (evolved.apply _).expects(lastIn).returns(lastYield)
      val resultStage = mock[Stage.Endo[String, Nothing]]("result stage")
      (lastOnDone.onComplete _).expects().returns(resultStage)
      val onDone = inside((lastYield, Loop(initial)(in))) {
        case (Yield.Some(lastOut, _, _), Yield.Some(resultOut, State.Success, onDone)) =>
          resultOut shouldBe lastOut
          onDone
        case (Yield.None(_, _), Yield.None(State.Success, onDone)) => onDone
      }
      val expectedStage = Loop(resultStage)
      onDone.onSuccess() shouldBe expectedStage
      onDone.onComplete() shouldBe expectedStage
      onDone.onError() shouldBe expectedStage
    }

  it should "be executed until the state is Error" in
    forAll(
      Gen.zip(
        Gen.listOf(Arbitrary.arbitrary[StateAndOnDoneToYieldSome[UUID, UUID, String]]),
        Arbitrary.arbitrary[StateAndOnDoneToYield[UUID, UUID, String]],
        Gen.uuid,
        Arbitrary.arbitrary[State.Error[String]]
      )) { case (yieldSuppliers, lastYieldSupplier, in, lastState) =>
      val initial = mock[Stage.Endo[UUID, String]]("initial stage")
      val (lastIn, evolved) = genStage(yieldSuppliers, initial, in)
      val lastOnDone = mock[OnDone[UUID, UUID, String]]
      val lastYield = lastYieldSupplier(lastState, lastOnDone)
      (evolved.apply _).expects(lastIn).returns(lastYield)
      val resultStage = mock[Stage.Endo[UUID, String]]("result stage")
      (lastOnDone.onError _).expects().returns(resultStage)
      val onDone = inside((lastYield, Loop(initial)(in))) {
        case (Yield.Some(lastOut, _, _), Yield.Some(resultOut, `lastState`, onDone)) =>
          resultOut shouldBe lastOut
          onDone
        case (Yield.None(_, _), Yield.None(`lastState`, onDone)) => onDone
      }
      val expectedStage = Loop(resultStage)
      onDone.onSuccess() shouldBe expectedStage
      onDone.onComplete() shouldBe expectedStage
      onDone.onError() shouldBe expectedStage
    }

  it should "be executed until the result is None" in
    forAll(
      Gen.zip(
        Gen.listOf(Arbitrary.arbitrary[StateAndOnDoneToYieldSome[BigInt, BigInt, Exception]]),
        Arbitrary.arbitrary[BigInt]
      )) { case (yieldSuppliers, in) =>
      val initial = mock[Stage.Endo[BigInt, Exception]]("initial stage")
      val (lastIn, evolved) = genStage(yieldSuppliers, initial, in)
      val lastOnDone = mock[OnDone[BigInt, BigInt, Exception]]("last OnDone")
      val lastYield = Yield.None(State.Success, lastOnDone)
      (evolved.apply _).expects(lastIn).returns(lastYield)
      val resultStage = mock[Stage.Endo[BigInt, Exception]]("result stage")
      (lastOnDone.onComplete _).expects().returns(resultStage)
      val onDone = inside((lastYield, Loop(initial)(in))) {
        case (Yield.None(_, _), Yield.None(State.Success, onDone)) => onDone
      }
      val expectedStage = Loop(resultStage)
      onDone.onSuccess() shouldBe expectedStage
      onDone.onComplete() shouldBe expectedStage
      onDone.onError() shouldBe expectedStage
    }

  @tailrec private def genStage[T: Arbitrary, E](
      yieldSuppliers: List[StateAndOnDoneToYieldSome[T, T, E]],
      stage: Stage.Endo[T, E], in: T): (T, Stage.Endo[T, E]) =
    yieldSuppliers match {
      case head :: tail =>
        val id = yieldSuppliers.length.toString
        val onDone = mock[OnDone[T, T, E]](s"onDone $id")
        val `yield` = head(State.Success, onDone)
        val evolved = mock[Stage.Endo[T, E]](s"stage $id")
        (onDone.onSuccess _).expects().returns(evolved)
        (stage.apply _).expects(in).returns(`yield`)
        genStage(tail, evolved, `yield`.out)
      case Nil => (in, stage)
    }

  "dispose" should "call stage's dispose" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    (stage.dispose _).expects()
    noException should be thrownBy Loop(stage).dispose()
  }
}
