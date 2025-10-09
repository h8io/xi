package h8io.xi.stages.util

import h8io.xi.stages.{Decorator, Stage, Yield}

object KeepLastOutput {
  private[util] final case class None[-I, +O, +E](stage: Stage[I, O, E]) extends Decorator[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      stage(in) match {
        case Yield.Some(out, signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
        case Yield.None(signal, onDone) => Yield.None(signal, onDone.map(None(_)))
      }
  }

  private[util] final case class Some[-I, +O, +E](out: O, stage: Stage[I, O, E]) extends Decorator[I, O, E] {
    def apply(in: I): Yield.Some[I, O, E] =
      stage(in) match {
        case Yield.Some(out, signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
        case Yield.None(signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
      }
  }

  def apply[I, O, E](stage: Stage[I, O, E]): Decorator[I, O, E] = None(stage)
}
