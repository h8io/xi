package h8io.xi.stages

import org.scalacheck.{Arbitrary, Gen}

trait StagesArbitraries extends SignalArbitraries {
  type SignalAndOnDoneToYieldSome[I, O, E] = (Signal[E], OnDone[I, O, E]) => Yield.Some[I, O, E]

  implicit def genSignalAndOnDoneToYieldSome[I, O: Arbitrary, E]: Arbitrary[SignalAndOnDoneToYieldSome[I, O, E]] =
    Arbitrary(Arbitrary.arbitrary[O] map { out => Yield.Some(out, _: Signal[E], _: OnDone[I, O, E]) })

  type SignalAndOnDoneToYield[I, O, E] = (Signal[E], OnDone[I, O, E]) => Yield[I, O, E]

  implicit def genSignalAndOnDoneToYield[I, O: Arbitrary, E]: Arbitrary[SignalAndOnDoneToYield[I, O, E]] =
    Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[SignalAndOnDoneToYieldSome[I, O, E]],
        Gen.const(Yield.None[I, O, E](_: Signal[E], _: OnDone[I, O, E]))))

  type OnDoneToYield[I, O, E] = OnDone[I, O, E] => Yield[I, O, E]

  implicit def genOnDoneToYield[I, O: Arbitrary, E: Arbitrary]: Arbitrary[OnDoneToYield[I, O, E]] =
    Arbitrary(
      for {
        yieldSupplier <- Arbitrary.arbitrary[SignalAndOnDoneToYield[I, O, E]]
        signal <- Arbitrary.arbitrary[Signal[E]]
      } yield yieldSupplier(signal, _: OnDone[I, O, E]))
}
