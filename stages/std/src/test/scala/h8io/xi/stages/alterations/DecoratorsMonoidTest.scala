package h8io.xi.stages.alterations

import cats.data.Chain
import cats.kernel.laws.discipline.MonoidTests
import cats.{Eq, Monoid}
import h8io.xi.stages.{Alteration, AlterationCompose, AlterationOps, Decorator, Stage}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

class DecoratorsMonoidTest extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  private implicit def decoratorMonoid[I, O, E]: Monoid[Decorator[I, O, E]] =
    new Monoid[Decorator[I, O, E]] {
      def empty: Decorator[I, O, E] = Identity[I, O, E]
      def combine(x: Decorator[I, O, E], y: Decorator[I, O, E]): Decorator[I, O, E] = x ~> y
    }

  private case class GeneratedAlteration[-IS <: Stage.Any, +OS <: Stage.Any](id: Long) extends Alteration[IS, OS] {
    override def apply(stage: IS): OS = throw new AbstractMethodError
  }

  private implicit def genDecorator[I, O, E]: Arbitrary[Decorator[I, O, E]] =
    Arbitrary(Gen.long map GeneratedAlteration[Stage[I, O, E], Stage[I, O, E]])

  // Depth should not be greater than 3
  def signature[IS <: Stage.Any, OS <: Stage.Any](alteration: Alteration[IS, OS]): Chain[Long] =
    alteration match {
      case GeneratedAlteration(id) => Chain(id)
      case Identity => Chain.empty
      case AlterationCompose(previous, next) => signature(previous) ++ signature(next)
      case other => throw new IllegalStateException(s"Unexpected decorator class: ${other.getClass}, value: $other")
    }

  private implicit def decoratorEq[I, O, E]: Eq[Decorator[I, O, E]] = (x, y) => signature(x) == signature(y)

  checkAll("Decorator", MonoidTests[Decorator[Any, Nothing, Nothing]].monoid)
}
