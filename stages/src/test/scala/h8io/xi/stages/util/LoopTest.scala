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
    forAll(Gen.zip(Gen.choose(0, 100), Arbitrary.arbitrary[Option[String]])) { case (n, in) =>
      val initial = mock[Stage.Endo[Option[String], Nothing]]("initial stage")
      val (lastIn, updated) = genStage(n, initial, in)
      val lastYield = genLastYield[String, Nothing](State.Complete)
      (updated.apply _).expects(lastIn).returns(lastYield)
      val resultStage = mock[Stage.Endo[Option[String], Nothing]]("result stage")
      (lastYield.onDone.onComplete _).expects().returns(resultStage)
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
    forAll(Gen.zip(Gen.choose(0, 100), Arbitrary.arbitrary[Option[UUID]], Arbitrary.arbitrary[State.Error[String]])) {
      case (n, in, lastState) =>
        val initial = mock[Stage.Endo[Option[UUID], String]]("initial stage")
        val (lastIn, updated) = genStage(n, initial, in)
        val lastYield = genLastYield[UUID, String](lastState)
        (updated.apply _).expects(lastIn).returns(lastYield)
        val resultStage = mock[Stage.Endo[Option[UUID], String]]("result stage")
        (lastYield.onDone.onError _).expects().returns(resultStage)
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
    forAll(Gen.zip(Gen.choose(0, 100), Arbitrary.arbitrary[Option[BigInt]])) { case (n, in) =>
      val initial = mock[Stage.Endo[Option[BigInt], Exception]]("initial stage")
      val (lastIn, updated) = genStage(n, initial, in)
      val lastYield = Yield.None(State.Success, mock[OnDone[Option[BigInt], Option[BigInt], Exception]]("last OnDone"))
      (updated.apply _).expects(lastIn).returns(lastYield)
      val resultStage = mock[Stage.Endo[Option[BigInt], Exception]]("result stage")
      (lastYield.onDone.onComplete _).expects().returns(resultStage)
      val onDone = inside((lastYield, Loop(initial)(in))) {
        case (Yield.None(_, _), Yield.None(State.Success, onDone)) => onDone
      }
      val expectedStage = Loop(resultStage)
      onDone.onSuccess() shouldBe expectedStage
      onDone.onComplete() shouldBe expectedStage
      onDone.onError() shouldBe expectedStage
    }

  @tailrec private def genStage[T: Arbitrary, E](
      i: Int,
      stage: Stage.Endo[Option[T], E],
      in: Option[T]): (Option[T], Stage.Endo[Option[T], E]) =
    if (i > 0) {
      val out = Arbitrary.arbitrary[T].sample
      val `yield` = Yield.Some(out, State.Success, mock[OnDone[Option[T], Option[T], E]](s"onDone $i"))
      val updated = mock[Stage.Endo[Option[T], E]](s"stage $i")
      (`yield`.onDone.onSuccess _).expects().returns(updated)
      (stage.apply _).expects(in).returns(`yield`)
      genStage(i - 1, updated, out)
    } else (in, stage)

  private def genLastYield[T: Arbitrary, E](state: State[E]): Yield[Option[T], Option[T], E] =
    Gen.prob(0.9).filter(identity).sample match {
      case Some(_) =>
        Yield.Some(Arbitrary.arbitrary[T].sample, state, mock[OnDone[Option[T], Option[T], E]]("last onDone"))
      case None => Yield.None(state, mock[OnDone[Option[T], Option[T], E]])
    }

  "dispose" should "call stage's dispose" in {
    val stage = mock[Stage[Any, Nothing, Nothing]]
    (stage.dispose _).expects()
    noException should be thrownBy Loop(stage).dispose()
  }
}
