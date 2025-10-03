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
    forAll(Gen.choose(0, 100), Gen.long) { (n, in) =>
      val initial = mock[Stage[Long, String, Nothing]]("initial stage")
      val updated = genStage(n, initial, in)
      val lastYield = genYield[Long, String, Nothing]("last", State.Complete)
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
    forAll(Gen.choose(0, 100), Arbitrary.arbitrary[Instant], Arbitrary.arbitrary[State.Error[Exception]]) {
      (n, in, lastState) =>
        val initial = mock[Stage[Instant, UUID, Exception]]("initial stage")
        val updated = genStage(n, initial, in)
        val lastYield = genYield[Instant, UUID, Exception]("last", lastState)
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

  @tailrec private def genStage[I, O: Arbitrary, E](i: Int, stage: Stage[I, O, E], in: I): Stage[I, O, E] =
    if (i > 0) {
      val `yield` = genYield[I, O, E](i.toString, State.Success)
      val updated = mock[Stage[I, O, E]](s"stage $i")
      (`yield`.onDone.onSuccess _).expects().returns(updated)
      (stage.apply _).expects(in).returns(`yield`)
      genStage(i - 1, updated, in)
    } else stage

  private def genYield[I, O: Arbitrary, E](id: String, state: State[E]): Yield[I, O, E] =
    Gen.prob(0.9).filter(identity).sample flatMap (_ => Arbitrary.arbitrary[O].sample) match {
      case Some(value) => Yield.Some(value, state, mock[OnDone[I, O, E]](s"onDone $id"))
      case None => Yield.None(state, mock[OnDone[I, O, E]])
    }

  "dispose" should "call stage's dispose" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    (stage.dispose _).expects()
    noException should be thrownBy Repeat(stage).dispose()
  }
}
