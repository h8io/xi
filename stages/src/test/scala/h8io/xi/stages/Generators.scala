package h8io.xi.stages

import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Gen}

trait Generators {
  implicit def genStateError[E: Arbitrary]: Arbitrary[State.Error[E]] =
    Arbitrary(catsLawsArbitraryForNonEmptyChain[E].arbitrary.map(State.Error(_)))

  implicit def genState[E: Arbitrary]: Arbitrary[State[E]] =
    Arbitrary(
      Gen.oneOf(Gen.const(State.Success: State[E]), Gen.const(State.Complete: State[E]), genStateError[E].arbitrary))
}
