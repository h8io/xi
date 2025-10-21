package h8io.xi.stages.alterators

import h8io.xi.stages.std.Fruitful
import h8io.xi.stages.{Stage, Wrapper, Yield}

object KeepLastOutput {
  private[alterators] final case class None[-I, +O, +E](stage: Stage[I, O, E]) extends Wrapper.Endo[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      stage(in) match {
        case Yield.Some(out, signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
        case Yield.None(signal, onDone) => Yield.None(signal, onDone.map(None(_)))
      }
  }

  private[alterators] final case class Some[-I, +O, +E](out: O, stage: Stage[I, O, E])
      extends Wrapper.Endo[I, O, E] with Fruitful[I, O, E] {
    def apply(in: I): Yield.Some[I, O, E] =
      stage(in) match {
        case Yield.Some(out, signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
        case Yield.None(signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
      }
  }

  def apply[I, O, E](stage: Stage[I, O, E]): Stage[I, O, E] = None(stage)
}
