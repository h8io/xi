package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Instant, OffsetDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.UUID

class MorphismTest extends AnyFlatSpec with Matchers with MockFactory {
  "∘" should "compose morphisms" in {
    val outer = mock[Morphism[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val inner = mock[Morphism[Stage[ZoneId, ZonedDateTime, Nothing], Stage[ZoneOffset, OffsetDateTime, Exception]]]
    outer ∘ inner shouldBe Morphism.Compose(outer, inner)
  }

  "~>" should "compose morphisms" in {
    val outer = mock[Morphism[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val inner = mock[Morphism[Stage[ZoneId, ZonedDateTime, Nothing], Stage[ZoneOffset, OffsetDateTime, Exception]]]
    outer ~> inner shouldBe outer ∘ inner
  }

  "<~" should "compose morphisms" in {
    val outer = mock[Morphism[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val inner = mock[Morphism[Stage[ZoneId, ZonedDateTime, Nothing], Stage[ZoneOffset, OffsetDateTime, Exception]]]
    inner <~ outer shouldBe outer ∘ inner
  }

  "⋅ and <|" should "apply a morphism to a stage" in {
    val morphism = mock[Morphism[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val stage = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val morphed = mock[Stage[UUID, Instant, Long]]
    (morphism.apply _).expects(stage).returns(morphed)
    morphism ⋅ stage shouldBe morphed
    (morphism.apply _).expects(stage).returns(morphed)
    morphism <| stage shouldBe morphed
  }

  "Compose" should "apply morphisms to a stage sequentially" in {
    val outer = mock[Morphism[Stage[ZoneOffset, OffsetDateTime, Exception], Stage[UUID, Instant, Long]]]
    val inner = mock[Morphism[Stage[ZoneId, ZonedDateTime, Nothing], Stage[ZoneOffset, OffsetDateTime, Exception]]]
    val input = mock[Stage[ZoneId, ZonedDateTime, Nothing]]
    val medium = mock[Stage[ZoneOffset, OffsetDateTime, Exception]]
    val output = mock[Stage[UUID, Instant, Long]]
    inSequence {
      (inner.apply _).expects(input).returns(medium)
      (outer.apply _).expects(medium).returns(output)
    }
    Morphism.Compose(outer, inner)(input) shouldBe output
  }
}
