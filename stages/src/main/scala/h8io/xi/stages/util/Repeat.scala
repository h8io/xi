package h8io.xi.stages.util

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](conditionThunk: () => Condition, stage: Stage[I, O, E])
    extends Stage.Safe[I, O, E] {
  override def apply(in: I): Yield[I, O, E] = {
    @tailrec
    def repeat(condition: Condition, stage: Stage[I, O, E], last: Outcome[I, O, E]): Yield[I, O, E] =
      if (condition.check) {
        val outcome = stage.safe(in).outcome()
        outcome.state match {
          case State.Success(next) => repeat(condition.advance(), next, outcome)
          case State.Complete(next) => outcome.toYield(State.Success(Repeat(condition.reset, next)))
          case failure => outcome.toYield(failure.map(Repeat(condition.reset, _)))
        }
      } else last.toYield(State.Success(Repeat(condition.reset, stage)))
    repeat(conditionThunk(), stage, Repeat.EmptyOutcome)
  }
}

object Repeat {
  private val EmptyOutcome: Outcome.None[Any, Nothing, Nothing] = Outcome.None(DeadEnd.State, { () => })

  def apply[I, O, E](stage: Stage[I, O, E]): Stage.Safe[I, O, E] = Repeat(Condition.True, stage)
}
