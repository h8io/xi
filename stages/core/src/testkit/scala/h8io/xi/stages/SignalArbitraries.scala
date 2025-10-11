package h8io.xi.stages

import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Gen}

trait SignalArbitraries {
  implicit def arbSignalError[E: Arbitrary]: Arbitrary[Signal.Error[E]] =
    Arbitrary(catsLawsArbitraryForNonEmptyChain[E].arbitrary.map(Signal.Error(_)))

  implicit def arbSignal[E: Arbitrary]: Arbitrary[Signal[E]] =
    Arbitrary(Gen.oneOf(Gen.const(Signal.Success: Signal[E]), Gen.const(Signal.Complete), arbSignalError[E].arbitrary))
}
