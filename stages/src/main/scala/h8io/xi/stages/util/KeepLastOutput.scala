package h8io.xi.stages.util

import h8io.xi.stages.{Stage, Yield}

object KeepLastOutput {
  private[util] final case class None[-I, +O, +E](stage: Stage[I, O, E]) extends Stage.Decorator[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      stage(in) match {
        case Yield.Some(out, state, onDone) => Yield.Some(out, state, onDone.map(Some(out, _)))
        case Yield.None(state, onDone) => Yield.None(state, onDone.map(None(_)))
      }
  }

  private[util] final case class Some[-I, +O, +E](out: O, stage: Stage[I, O, E]) extends Stage.Decorator[I, O, E] {
    def apply(in: I): Yield.Some[I, O, E] =
      stage(in) match {
        case Yield.Some(out, state, onDone) => Yield.Some(out, state, onDone.map(Some(out, _)))
        case Yield.None(state, onDone) => Yield.Some(out, state, onDone.map(Some(out, _)))
      }
  }

  def apply[I, O, E](stage: Stage[I, O, E]): Stage.Decorator[I, O, E] = None(stage)
}
