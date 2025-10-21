package h8io.xi.stages.alterators

import cats.kernel.laws.discipline.MonoidTests
import cats.{Eq, Monoid}
import h8io.xi.stages.{Alterator, AlteratorCompose, AlteratorOps, Decorator, Stage}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import scala.annotation.tailrec

class DecoratorsMonoidTest extends AnyFunSuite with FunSuiteDiscipline with Checkers {
  private implicit def decoratorMonoid[I, O, E]: Monoid[Decorator[I, O, E]] =
    new Monoid[Decorator[I, O, E]] {
      def empty: Decorator[I, O, E] = Identity[I, O, E]
      def combine(x: Decorator[I, O, E], y: Decorator[I, O, E]): Decorator[I, O, E] = x ~> y
    }

  private case class GeneratedAlterator[-IS <: Stage.Any, +OS <: Stage.Any](id: Long) extends Alterator[IS, OS] {
    override def apply(stage: IS): OS = throw new AbstractMethodError
  }

  private implicit def genDecorator[I, O, E]: Arbitrary[Decorator[I, O, E]] =
    Arbitrary(Gen.long map GeneratedAlterator[Stage[I, O, E], Stage[I, O, E]])

  def signature(alterator: Alterator[?, ?]): List[Long] = {
    @tailrec def loop(todo: List[Alterator[?, ?]], acc: List[Long]): List[Long] =
      todo match {
        case Nil => acc
        case Identity :: rest => loop(rest, acc)
        case GeneratedAlterator(id) :: rest => loop(rest, id :: acc)
        case AlteratorCompose(previous, next) :: rest => loop(next :: previous :: rest, acc)
        case other => fail(s"Unexpected decorator class: ${other.getClass}, value: $other")
      }
    loop(alterator :: Nil, Nil)
  }

  private implicit def decoratorEq[I, O, E]: Eq[Decorator[I, O, E]] = (x, y) => signature(x) == signature(y)

  checkAll("Decorator", MonoidTests[Decorator[Any, Nothing, Nothing]].monoid)
}
