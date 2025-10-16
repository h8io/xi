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
    val morphee = mock[Stage[ZoneId, ZonedDateTime, String]]
    val morphed = mock[Stage[Long, String, Nothing]]
    (alteration.apply _).expects(morphee).returns(morphed)
    val result: Stage[Int, String, UUID] = (stage ~> alteration)(morphee)
    result shouldBe stage ~> morphed
    (alteration.apply _).expects(morphee).returns(morphed)
    stage ~> alteration <| morphee shouldBe stage ~> morphed
  }

  "<~" should "produce Stage.AndThen object" in {
    val previous = mock[Stage[Instant, Int, String]]
    val next = mock[Stage[Int, String, Nothing]]
    next <~ previous shouldBe Stage.AndThen(previous, next)
  }

  "|>" should "apply alteration to stage" in {
    val stage = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val alteration = mock[Alteration[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val morphed = mock[Stage[UUID, Instant, Long]]
    (alteration.apply _).expects(stage).returns(morphed)
    stage |> alteration shouldBe morphed
  }

  "dispose" should "do nothing" in {
    noException should be thrownBy new Stage[Instant, Timestamp, String] {
      def apply(in: Instant): Yield[Instant, Timestamp, String] = throw new NotImplementedError
    }.dispose()
  }
}
