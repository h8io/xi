package h8io.xi.stages.std

import cats.data.NonEmptyChain
import h8io.xi.stages.{OnDone, Stage, State, Yield}
import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GlobalCountdownTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "GlobalCountdown" should "return DeadEnd.Yield if the counter is zero" in {
    GlobalCountdown(0, mock[Stage[Long, Array[Byte], String]])(13) shouldBe DeadEnd.Yield
  }

  it should "run three times and then return DeadEnd.Yield" in {
    val stage1 = mock[Stage[String, Int, String]]("Stage 1")
    val cdStage1 = GlobalCountdown(3, stage1)
    val onDone1 = mock[OnDone[String, Int, String]]("OnDone 1")
    (stage1.apply _).expects("xi").returns(Yield.Some(42, onDone1))

    val (cdOnDone1, stage2) = inside(cdStage1("xi")) { case Yield.Some(42, onDone) =>
      val stage = mock[Stage[String, Int, String]]("Stage 2")
      (onDone1.onSuccess _).expects().returns(State.Success(stage))
      (onDone, stage)
    }

    val (cdStage2, onDone2) = inside(cdOnDone1.onSuccess()) { case State.Success(stage) =>
      stage shouldBe a[GlobalCountdown[?, ?, ?]]
      val onDone = mock[OnDone[String, Int, String]]("OnDone 2")
      (stage2.apply _).expects("query").returns(Yield.Some(17, onDone))
      (stage, onDone)
    }

    val (cdOnDone2, stage3) = inside(cdStage2("query")) { case Yield.Some(17, onDone) =>
      val stage = mock[Stage[String, Int, String]]("Stage 3")
      (onDone2.onComplete _).expects().returns(State.Complete(stage))
      (onDone, stage)
    }

    val (cdStage3, onDone3) = inside(cdOnDone2.onComplete()) { case State.Complete(stage) =>
      stage shouldBe a[GlobalCountdown[?, ?, ?]]
      val onDone = mock[OnDone[String, Int, String]]("OnDone 3")
      (stage3.apply _).expects("language").returns(Yield.Some(3, onDone))
      (stage, onDone)
    }

    inside(cdStage3("language")) { case Yield.Some(3, onDone) =>
      (onDone3.onError _).expects().returns(State.Error(mock[Stage[String, Int, String]]("Stage 4"), "error"))
      inside(onDone.onError()) { case State.Error(stage, errors) =>
        stage shouldBe a[GlobalCountdown[?, ?, ?]]
        errors shouldBe NonEmptyChain.one("error")
        stage("dead end") shouldBe DeadEnd.Yield
      }
    }
  }
}
