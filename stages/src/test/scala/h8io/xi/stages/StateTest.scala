package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Instant

class StateTest extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks with Generators {
  "Success" should "be idempotent" in { State.Success ~> State.Success shouldBe State.Success }

  it should "call the method onSuccess() in OnDone object" in {
    val onDone = mock[OnDone[Long, Instant, Exception]]
    val stage = mock[Stage[Long, Instant, Exception]]
    (onDone.onSuccess _).expects().returns(stage)
    State.Success(onDone) shouldBe stage
  }

  "Complete" should "be idempotent" in { State.Complete ~> State.Complete shouldBe State.Complete }

  it should "be overridden by Error" in
    forAll((error: State.Error[String]) => State.Complete ~> error shouldBe error)

  it should "call the method onComplete() in OnDone object" in {
    val onDone = mock[OnDone[Long, Instant, Exception]]
    val stage = mock[Stage[Long, Instant, Exception]]
    (onDone.onComplete _).expects().returns(stage)
    State.Complete(onDone) shouldBe stage
  }

  "Error" should "keep the order of causes in composition" in
    forAll { (previous: State.Error[String], next: State.Error[String]) =>
      previous ~> next shouldBe State.Error(previous.causes ++ next.causes)
    }

  it should "override Complete" in forAll((error: State.Error[String]) => error ~> State.Complete shouldBe error)

  it should "call the onError() method in OnDone object" in
    forAll { (error: State.Error[String]) =>
      val onDone = mock[OnDone[Long, Instant, Exception]]
      val stage = mock[Stage[Long, Instant, Exception]]
      (onDone.onError _).expects().returns(stage)
      error(onDone) shouldBe stage
    }
}
