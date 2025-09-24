package h8io.xi.stages.std

import h8io.xi.stages.{Stage, State, Yield}

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
  override def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stage: Stage[I, O, E]): Yield[I, O, E] = {
      val outcome = stage.safe(in).outcome()
      outcome.state match {
        case State.Success(next) => repeat(next)
        case State.Complete(next) => outcome.toYield(State.Success(Repeat(next)))
        case failure => outcome.toYield(failure.map(Repeat(_)))
      }
    }
    repeat(stage)
  }
}
