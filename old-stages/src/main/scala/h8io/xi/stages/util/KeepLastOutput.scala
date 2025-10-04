package h8io.xi.stages.util

import h8io.xi.stages.{Stage, Yield}

object KeepLastOutput {
  private[util] final case class None[-I, +O, +E](stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      stage.safe(in) match {
        case Yield.Some(out, onDone) => Yield.Some(out, onDone.lift(Some(out, _)))
        case Yield.None(onDone) => Yield.None(onDone.lift(None(_)))
      }
  }

  private[util] final case class Some[-I, +O, +E](out: O, stage: Stage[I, O, E]) extends Stage.Safe[I, O, E] {
    def apply(in: I): Yield.Some[I, O, E] =
      stage.safe(in) match {
        case Yield.Some(out, onDone) => Yield.Some(out, onDone.lift(Some(out, _)))
        case Yield.None(onDone) => Yield.Some(out, onDone.lift(Some(out, _)))
      }
  }

  def apply[I, O, E](stage: Stage[I, O, E]): Stage.Safe[I, O, E] = None(stage)
}
