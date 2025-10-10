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
        signal <- Arbitrary.arbitrary[Signal[E]]
      } yield new Stage.Endo[T, E] {
        def apply(in: T): Yield[T, T, E] = Yield.Some(prefix |+| in |+| suffix, signal, OnDone.FromStage(this))

        override def toString(): String = s"Stage.Endo: $prefix + _ + $suffix"
      }
    }

  private implicit def stageMonoid[T, E]: Monoid[Stage.Endo[T, E]] =
    new Monoid[Stage.Endo[T, E]] {
      def empty: Stage.Endo[T, E] = Identity[T]
      def combine(x: Stage.Endo[T, E], y: Stage.Endo[T, E]): Stage.Endo[T, E] = x ~> y
    }

  // Depth should not be greater than 3
  private def toList[E](stage: Stage[?, ?, E]): List[Stage[?, ?, E]] =
    stage match {
      case AndThen(previous, next) => toList(previous) ++ toList(next)
      case Identity => Nil
      case _ => stage :: Nil
    }

  private def toTuple[T, E](
      onDone: OnDone[T, T, E]): (List[Stage[?, ?, ?]], List[Stage[?, ?, ?]], List[Stage[?, ?, ?]]) =
    (toList(onDone.onSuccess()), toList(onDone.onError()), toList(onDone.onComplete()))

  private def toTuple[T, E](`yield`: Yield[T, T, E]): Product =
    `yield` match {
      case Yield.Some(out, signal, onDone) => (out, signal, toTuple(onDone))
      case Yield.None(signal, onDone) => (signal, toTuple(onDone))
    }

  private implicit def stageEq[T: Arbitrary: Monoid, E]: Eq[Stage.Endo[T, E]] = {
    (x: Stage.Endo[T, E], y: Stage.Endo[T, E]) =>
      val in = Arbitrary.arbitrary[T].sample getOrElse Monoid[T].empty
      toTuple(x(in)) == toTuple(y(in))
  }

  checkAll("Stage[Int, String]", MonoidTests[Stage.Endo[Int, String]].monoid)

  checkAll("Stage[String, Int]", MonoidTests[Stage.Endo[Int, String]].monoid)
}
