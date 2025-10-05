package h8io.xi.stages.util

import h8io.xi.stages.*
import org.scalacheck.{Arbitrary, Gen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Instant
import java.util.UUID
import scala.annotation.tailrec

class RepeatTest
    extends AnyFlatSpec with Matchers with Inside with MockFactory with ScalaCheckPropertyChecks with Generators {
  "Repeat" should "be executed until the state is Complete" in
    forAll(Gen.zip(Gen.nonEmptyListOf(Arbitrary.arbitrary[StateAndOnDoneToYield[Long, String, Nothing]]), Gen.long)) {
      case (yieldSuppliers, in) =>
        val initial = mock[Stage[Long, String, Nothing]]("initial stage")
        val updated = createStage(yieldSuppliers.tail, initial, in)
        val lastYield = genYield[Long, String, Nothing]("last", yieldSuppliers.head, State.Complete)
        (updated.apply _).expects(in).returns(lastYield)
        val resultStage = mock[Stage[Long, String, Nothing]]("result stage")
        (lastYield.onDone.onComplete _).expects().returns(resultStage)
        val onDone = inside((lastYield, Repeat(initial)(in))) {
          case (Yield.Some(lastOut, _, _), Yield.Some(resultOut, State.Success, onDone)) =>
            resultOut shouldBe lastOut
            onDone
          case (Yield.None(_, _), Yield.None(State.Success, onDone)) => onDone
        }
        val expectedStage = Repeat(resultStage)
        onDone.onSuccess() shouldBe expectedStage
        onDone.onComplete() shouldBe expectedStage
        onDone.onError() shouldBe expectedStage
    }

  it should "be executed until the state is Error" in
    forAll(
      Gen.zip(
        Gen.nonEmptyListOf(Arbitrary.arbitrary[StateAndOnDoneToYield[Instant, UUID, Exception]]),
        Arbitrary.arbitrary[Instant],
        Arbitrary.arbitrary[State.Error[Exception]]
      )) {
      case (yieldSuppliers, in, lastState) =>
        val initial = mock[Stage[Instant, UUID, Exception]]("initial stage")
        val updated = createStage(yieldSuppliers.tail, initial, in)
        val lastYield = genYield[Instant, UUID, Exception]("last", yieldSuppliers.head, lastState)
        (updated.apply _).expects(in).returns(lastYield)
        val resultStage = mock[Stage[Instant, UUID, Exception]]("result stage")
        (lastYield.onDone.onError _).expects().returns(resultStage)
        val onDone = inside((lastYield, Repeat(initial)(in))) {
          case (Yield.Some(lastOut, _, _), Yield.Some(resultOut, `lastState`, onDone)) =>
            resultOut shouldBe lastOut
            onDone
          case (Yield.None(_, _), Yield.None(`lastState`, onDone)) => onDone
        }
        val expectedStage = Repeat(resultStage)
        onDone.onSuccess() shouldBe expectedStage
        onDone.onComplete() shouldBe expectedStage
        onDone.onError() shouldBe expectedStage
    }

  @tailrec private def createStage[I, O, E](
      yieldSuppliers: List[StateAndOnDoneToYield[I, O, E]],
      stage: Stage[I, O, E], in: I): Stage[I, O, E] =
    yieldSuppliers match {
      case out :: tail =>
        val id = yieldSuppliers.length.toString
        val `yield` = genYield[I, O, E](id, out, State.Success)
        val updated = mock[Stage[I, O, E]](s"stage $id")
        (`yield`.onDone.onSuccess _).expects().returns(updated)
        (stage.apply _).expects(in).returns(`yield`)
        createStage(tail, updated, in)
      case Nil => stage
    }

  private def genYield[I, O, E](
      id: String,
      yieldSupplier: StateAndOnDoneToYield[I, O, E], state: State[E]): Yield[I, O, E] =
    yieldSupplier(state, mock[OnDone[I, O, E]](s"onDone $id"))

  "dispose" should "call stage's dispose" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    (stage.dispose _).expects()
    noException should be thrownBy Repeat(stage).dispose()
  }
}
