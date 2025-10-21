package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.*
import java.util.UUID

class AlteratorTest extends AnyFlatSpec with Matchers with MockFactory {
  "∘" should "compose alterators" in {
    val outer = mock[Alterator[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val inner = mock[Alterator[Stage[ZoneId, ZonedDateTime, Nothing], Stage[ZoneOffset, OffsetDateTime, Exception]]]
    outer ∘ inner shouldBe AlteratorCompose(outer, inner)
  }

  "arrows" should "compose alterators" in {
    val outer = mock[Alterator[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val inner = mock[Alterator[Stage[ZoneId, ZonedDateTime, Nothing], Stage[ZoneOffset, OffsetDateTime, Exception]]]
    outer ~> inner shouldBe outer ∘ inner
    outer <| inner shouldBe outer ∘ inner
    inner <~ outer shouldBe outer ∘ inner
  }

  "⋅ and <|" should "apply a alterator to a stage" in {
    val alterator = mock[Alterator[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val stage = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val in = mock[Stage[UUID, Instant, Long]]
    (alterator.apply _).expects(stage).returns(in)
    alterator ⋅ stage shouldBe in
    (alterator.apply _).expects(stage).returns(in)
    alterator <| stage shouldBe in
  }

  "AlteratorCompose" should "apply alterators to a stage sequentially" in {
    val outer = mock[Alterator[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val inner = mock[Alterator[Stage[ZoneId, ZonedDateTime, Nothing], Stage[ZoneOffset, OffsetDateTime, Exception]]]
    val input = mock[Stage[ZoneId, ZonedDateTime, Nothing]]
    val medium = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val output = mock[Stage[UUID, Instant, Long]]
    inSequence {
      (inner.apply _).expects(input).returns(medium)
      (outer.apply _).expects(medium).returns(output)
    }
    AlteratorCompose(outer, inner)(input) shouldBe output
  }
}
