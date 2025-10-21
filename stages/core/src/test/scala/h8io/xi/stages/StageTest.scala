package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.sql.Timestamp
import java.time.*
import java.util.UUID

class StageTest extends AnyFlatSpec with Matchers with MockFactory {
  "~>" should "produce Stage.AndThen object with a stage argument" in {
    val previous = mock[Stage[String, Long, Nothing]]
    val next = mock[Stage[Long, Timestamp, String]]
    previous ~> next shouldBe Stage.AndThen(previous, next)
  }

  it should "produce a alterator with a alterator argument" in {
    val stage = mock[Stage[Int, Long, UUID]]
    val alterator = mock[Alterator[Stage[ZoneId, ZonedDateTime, String], Stage[Long, String, Nothing]]]
    val in = mock[Stage[ZoneId, ZonedDateTime, String]]
    val out = mock[Stage[Long, String, Nothing]]
    (alterator.apply _).expects(in).returns(out)
    val result: Stage[Int, String, UUID] = (stage ~> alterator)(in)
    result shouldBe stage ~> out
    (alterator.apply _).expects(in).returns(out)
    stage ~> alterator <| in shouldBe stage ~> out
  }

  "<~" should "produce Stage.AndThen object" in {
    val previous = mock[Stage[Instant, Int, String]]
    val next = mock[Stage[Int, String, Nothing]]
    next <~ previous shouldBe Stage.AndThen(previous, next)
  }

  "|>" should "apply alterator to stage" in {
    val stage = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val alterator = mock[Alterator[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val out = mock[Stage[UUID, Instant, Long]]
    (alterator.apply _).expects(stage).returns(out)
    stage |> alterator shouldBe out
  }

  "dispose" should "do nothing" in {
    noException should be thrownBy new Stage[Instant, Timestamp, String] {
      def apply(in: Instant): Yield[Instant, Timestamp, String] = throw new NotImplementedError
    }.dispose()
  }

  "alterator" should "be a leftAlterator" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    left.alterator(right) shouldBe left ~> right
  }

  "leftAlterator" should "produce a composition with predefined left operand as an be alterator" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    val alterator: Alterator[Stage[Long, Duration, Exception], Stage[Int, Duration, Exception]] = left.leftAlterator
    alterator(right) shouldBe left ~> right
  }

  "rightAlterator" should "produce a composition with predefined right operand as an be alterator" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    val alterator: Alterator[Stage[Int, Long, Nothing], Stage[Int, Duration, Exception]] = right.rightAlterator
    alterator(left) shouldBe left ~> right
  }
}
