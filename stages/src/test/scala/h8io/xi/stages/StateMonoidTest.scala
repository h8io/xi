package h8io.xi.stages

import cats.kernel.laws.discipline.MonoidTests
import cats.{Eq, Monoid}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class StateMonoidTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with Generators {
  private implicit def stateMonoid[E]: Monoid[State[E]] =
    new Monoid[State[E]] {
      def empty: State[E] = State.Success
      def combine(x: State[E], y: State[E]): State[E] = x ~> y
    }

  private implicit def stateEq[E]: Eq[State[E]] = Eq.fromUniversalEquals[State[E]]

  checkAll("State[Int]", MonoidTests[State[Int]].monoid)
}
