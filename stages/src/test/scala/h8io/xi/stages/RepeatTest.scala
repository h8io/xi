package h8io.xi.stages

import org.scalamock.scalatest.proxy.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RepeatTest extends AnyFlatSpec with Matchers with MockFactory {
  "Repeat" should "be executed until the state `Complete`" in {}

  it should "be executed until the state `Failure`" in {}
}
