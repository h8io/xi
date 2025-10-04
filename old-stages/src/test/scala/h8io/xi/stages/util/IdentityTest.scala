package h8io.xi.stages.util

import h8io.xi.stages.{State, Yield}
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class IdentityTest extends AnyFlatSpec with Matchers with Inside {
  "Identity" should "return input" in {
    val in = Random.nextString(Random.nextInt(100))
    val id = Identity[String]
    inside(id(in)) { case Yield.Some(`in`, onDone) =>
      val state = State.Success(id)
      onDone.onSuccess() shouldBe state
      onDone.onComplete() shouldBe state
      onDone.onError() shouldBe state
      onDone.onPanic() shouldBe state
      onDone.dispose()
    }
  }
}
