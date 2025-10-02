package h8io.xi.stages

import cats.implicits.catsSyntaxSemigroup
import cats.kernel.laws.discipline.MonoidTests
import cats.{Eq, Monoid, Semigroup}
import h8io.xi.stages.util.Identity
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class EndoStageMonoidTest extends AnyFunSuite with FunSuiteDiscipline with Checkers with Generators {
  private implicit def genStage[T: Arbitrary: Semigroup, E: Arbitrary]: Arbitrary[Stage.Endo[T, E]] =
    Arbitrary {
      for {
        prefix <- Arbitrary.arbitrary[T]
        suffix <- Arbitrary.arbitrary[T]
        state <- Arbitrary.arbitrary[State[E]]
      } yield new Stage.Endo[T, E] {
        def apply(in: T): Yield[T, T, E] = Yield.Some(prefix |+| in |+| suffix, state, OnDone.FromStage(this))

        override def toString(): String = s"Stage.Endo: $prefix + _ + $suffix"
      }
    }

  private implicit def stageMonoid[T, E]: Monoid[Stage.Endo[T, E]] =
    new Monoid[Stage.Endo[T, E]] {
      def empty: Stage.Endo[T, E] = Identity[T]
      def combine(x: Stage.Endo[T, E], y: Stage.Endo[T, E]): Stage.Endo[T, E] = x ~> y
    }

  private def toList(stage: Stage[?, ?, ?]): List[Stage[?, ?, ?]] =
    stage match {
      case Stage.AndThen(previous, next) => toList(previous) ++ toList(next)
      case Identity => Nil
      case _ => stage :: Nil
    }

  private def toTuple[T, E](
      onDone: OnDone[T, T, E]): (List[Stage[?, ?, ?]], List[Stage[?, ?, ?]], List[Stage[?, ?, ?]]) =
    (toList(onDone.onSuccess()), toList(onDone.onError()), toList(onDone.onComplete()))

  private def toTuple[T, E](`yield`: Yield[T, T, E]): Product =
    `yield` match {
      case Yield.Some(out, state, onDone) => (out, state, toTuple(onDone))
      case Yield.None(state, onDone) => (state, toTuple(onDone))
    }

  private implicit def stageEq[T: Arbitrary: Monoid, E]: Eq[Stage.Endo[T, E]] = {
    (x: Stage.Endo[T, E], y: Stage.Endo[T, E]) =>
      val in = Arbitrary.arbitrary[T].sample match {
        case Some(value) => value
        case None => Monoid[T].empty
      }
      toTuple(x(in)) == toTuple(y(in))
  }

  checkAll("Stage[Int, String]", MonoidTests[Stage.Endo[Int, String]].monoid)

  checkAll("Stage[String, Int]", MonoidTests[Stage.Endo[Int, String]].monoid)
}
