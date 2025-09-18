package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LoopTest extends AnyFlatSpec with Matchers with Inside with MockFactory {
  "Loop" should "be executed until the result is Yield.None" in {}

  it should "be executed until the state is Complete" in {}

  it should "be executed until the state is Error" in {}

  it should "be executed until the state is Panic" in {}
}
