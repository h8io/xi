package h8io.xi.stages

import cats.data.NonEmptyChain
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.Instant

class SignalTest
    extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks with SignalArbitraries {
  "Success" should "be idempotent" in { Signal.Success ~> Signal.Success shouldBe Signal.Success }

  it should "call the method onSuccess() in OnDone object" in {
    val onDone = mock[OnDone[Long, Instant, Exception]]
    val stage = mock[Stage[Long, Instant, Exception]]
    (onDone.onSuccess _).expects().returns(stage)
    Signal.Success(onDone) shouldBe stage
  }

  it should "become Complete on break call" in { Signal.Success.break shouldBe Signal.Complete }

  "Complete" should "be idempotent" in { Signal.Complete ~> Signal.Complete shouldBe Signal.Complete }

  it should "be overridden by Error" in
    forAll((error: Signal.Error[String]) => Signal.Complete ~> error shouldBe error)

  it should "call the method onComplete() in OnDone object" in {
    val onDone = mock[OnDone[Long, Instant, Exception]]
    val stage = mock[Stage[Long, Instant, Exception]]
    (onDone.onComplete _).expects().returns(stage)
    Signal.Complete(onDone) shouldBe stage
  }

  it should "not change on break call" in { Signal.Complete.break shouldBe Signal.Complete }

  "Error" should "keep the order of causes in composition" in
    forAll { (previous: Signal.Error[String], next: Signal.Error[String]) =>
      previous ~> next shouldBe Signal.Error(previous.causes ++ next.causes)
    }

  it should "override Complete" in forAll((error: Signal.Error[String]) => error ~> Signal.Complete shouldBe error)

  it should "call the onError() method in OnDone object" in
    forAll { (error: Signal.Error[String]) =>
      val onDone = mock[OnDone[Long, Instant, Exception]]
      val stage = mock[Stage[Long, Instant, Exception]]
      (onDone.onError _).expects().returns(stage)
      error(onDone) shouldBe stage
    }

  it should "create Error signal with a single error" in {
    val error = mock[AnyRef]
    Signal.error(error) shouldBe Signal.Error(NonEmptyChain.of(error))
  }

  it should "not change on break call" in forAll((error: Signal.Error[String]) => error.break shouldBe error)
}
