package h8io.xi.stages

import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Gen}

trait Generators {
  implicit def genStateError[E: Arbitrary]: Arbitrary[State.Error[E]] =
    Arbitrary(catsLawsArbitraryForNonEmptyChain[E].arbitrary.map(State.Error(_)))

  implicit def genState[E: Arbitrary]: Arbitrary[State[E]] =
    Arbitrary(Gen.oneOf(Gen.const(State.Success: State[E]), Gen.const(State.Complete), genStateError[E].arbitrary))

  type StateAndOnDoneToYieldSome[I, O, E] = (State[E], OnDone[I, O, E]) => Yield.Some[I, O, E]

  implicit def genStateAndOnDoneToYieldSome[I, O: Arbitrary, E]: Arbitrary[StateAndOnDoneToYieldSome[I, O, E]] =
    Arbitrary(Arbitrary.arbitrary[O] map { out => Yield.Some(out, _: State[E], _: OnDone[I, O, E]) })

  type StateAndOnDoneToYield[I, O, E] = (State[E], OnDone[I, O, E]) => Yield[I, O, E]

  implicit def genStateAndOnDoneToYield[I, O: Arbitrary, E]: Arbitrary[StateAndOnDoneToYield[I, O, E]] =
    Arbitrary(
      Gen.oneOf(
        Arbitrary.arbitrary[StateAndOnDoneToYieldSome[I, O, E]],
        Gen.const(Yield.None[I, O, E](_: State[E], _: OnDone[I, O, E]))))

  type OnDoneToYield[I, O, E] = OnDone[I, O, E] => Yield[I, O, E]

  implicit def genOnDoneToYield[I, O: Arbitrary, E: Arbitrary]: Arbitrary[OnDoneToYield[I, O, E]] =
    Arbitrary(
      for {
        yieldSupplier <- Arbitrary.arbitrary[StateAndOnDoneToYield[I, O, E]]
        state <- Arbitrary.arbitrary[State[E]]
      } yield yieldSupplier(state, _: OnDone[I, O, E]))
}
