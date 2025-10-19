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

  it should "produce a alteration with a alteration argument" in {
    val stage = mock[Stage[Int, Long, UUID]]
    val alteration = mock[Alteration[Stage[ZoneId, ZonedDateTime, String], Stage[Long, String, Nothing]]]
    val in = mock[Stage[ZoneId, ZonedDateTime, String]]
    val out = mock[Stage[Long, String, Nothing]]
    (alteration.apply _).expects(in).returns(out)
    val result: Stage[Int, String, UUID] = (stage ~> alteration)(in)
    result shouldBe stage ~> out
    (alteration.apply _).expects(in).returns(out)
    stage ~> alteration <| in shouldBe stage ~> out
  }

  "<~" should "produce Stage.AndThen object" in {
    val previous = mock[Stage[Instant, Int, String]]
    val next = mock[Stage[Int, String, Nothing]]
    next <~ previous shouldBe Stage.AndThen(previous, next)
  }

  "|>" should "apply alteration to stage" in {
    val stage = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val alteration = mock[Alteration[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val out = mock[Stage[UUID, Instant, Long]]
    (alteration.apply _).expects(stage).returns(out)
    stage |> alteration shouldBe out
  }

  "dispose" should "do nothing" in {
    noException should be thrownBy new Stage[Instant, Timestamp, String] {
      def apply(in: Instant): Yield[Instant, Timestamp, String] = throw new NotImplementedError
    }.dispose()
  }

  "alteration" should "be a leftAlteration" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    left.alteration(right) shouldBe left ~> right
  }

  "leftAlteration" should "produce a composition with predefined left operand as an be alteration" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    val alteration: Alteration[Stage[Long, Duration, Exception], Stage[Int, Duration, Exception]] = left.leftAlteration
    alteration(right) shouldBe left ~> right
  }

  "rightAlteration" should "produce a composition with predefined right operand as an be alteration" in {
    val left = mock[Stage[Int, Long, Nothing]]
    val right = mock[Stage[Long, Duration, Exception]]
    val alteration: Alteration[Stage[Int, Long, Nothing], Stage[Int, Duration, Exception]] = right.rightAlteration
    alteration(left) shouldBe left ~> right
  }
}
