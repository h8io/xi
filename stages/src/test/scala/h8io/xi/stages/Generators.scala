package h8io.xi.stages

import cats.laws.discipline.arbitrary.*
import org.scalacheck.{Arbitrary, Gen}

trait Generators {
  implicit def genStateError[E: Arbitrary]: Arbitrary[State.Error[E]] =
    Arbitrary(catsLawsArbitraryForNonEmptyChain[E].arbitrary.map(State.Error(_)))

  implicit def genState[E: Arbitrary]: Arbitrary[State[E]] =
    Arbitrary(Gen.oneOf(Gen.const(State.Success: State[E]), Gen.const(State.Complete), genStateError[E].arbitrary))

  type YieldSupplier[I, O, E] = OnDone[I, O, E] => Yield[I, O, E]

  implicit def genYieldSupplier[I, O: Arbitrary, E: Arbitrary]: Arbitrary[YieldSupplier[I, O, E]] =
    Arbitrary(
      for {
        isSome <- Arbitrary.arbitrary[Boolean]
        state <- Arbitrary.arbitrary[State[E]]
        yieldGen =
          if (isSome) Arbitrary.arbitrary[O] map { out => Yield.Some(out, state, _: OnDone[I, O, E]) }
          else Gen.const(Yield.None(state, _: OnDone[I, O, E]))
        yld <- yieldGen
      } yield yld)
}
