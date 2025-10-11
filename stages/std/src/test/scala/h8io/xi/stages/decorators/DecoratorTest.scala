package h8io.xi.stages.decorators

import h8io.xi.stages.{Stage, Yield}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecoratorTest extends AnyFlatSpec with Matchers with MockFactory {
  "Decorator's dispose" should "call underlying stage's dispose method" in {
    val underlying = mock[Stage[Any, Nothing, Nothing]]
    (underlying.dispose _).expects()
    noException should be thrownBy new Decorator[Any, Nothing, Nothing] {
      val stage: Stage[Any, Nothing, Nothing] = underlying
      def apply(in: Any): Yield[Any, Nothing, Nothing] = throw new NoSuchMethodError
    }.dispose()
  }
}
