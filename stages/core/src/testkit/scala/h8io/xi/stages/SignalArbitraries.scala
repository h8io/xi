package h8io.xi.stages

import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Gen}

trait SignalArbitraries {
  implicit def genSignalError[E: Arbitrary]: Arbitrary[Signal.Error[E]] =
    Arbitrary(catsLawsArbitraryForNonEmptyChain[E].arbitrary.map(Signal.Error(_)))

  implicit def genSignal[E: Arbitrary]: Arbitrary[Signal[E]] =
    Arbitrary(Gen.oneOf(Gen.const(Signal.Success: Signal[E]), Gen.const(Signal.Complete), genSignalError[E].arbitrary))
}
