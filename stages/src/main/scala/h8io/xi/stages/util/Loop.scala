package h8io.xi.stages.util

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Loop[T, +E](conditionThunk: () => Condition, stage: Stage.Endo[T, E]) extends Stage.SafeEndo[T, E] {
  def apply(in: T): Yield[T, T, E] = {
    @tailrec def loop(condition: Condition, stage: Stage[T, T, E], in: T, dispose: () => Unit): Yield[T, T, E] =
      if (condition.check) {
        val outcome = stage.safe(in).outcome()
        outcome.state match {
          case State.Success(updated) =>
            outcome match {
              case Outcome.Some(out, _, _) => loop(condition.advance(), updated, out, outcome.dispose)
              case Outcome.None(_, _) => outcome.toYield(State.Success(Loop(condition.reset, updated)))
            }
          case State.Complete(updated) => outcome.toYield(State.Success(Loop(condition.reset, updated)))
          case failure => outcome.toYield(failure.map(Loop(condition.reset, _)))
        }
      } else Yield.Some(in, State.Success(Loop(condition.reset, stage)).onDone(dispose))

    loop(conditionThunk(), stage, in, { () => })
  }
}

object Loop {
  def apply[I, E](stage: Stage.Endo[I, E]): Loop[I, E] = Loop(Condition.True, stage)
}
