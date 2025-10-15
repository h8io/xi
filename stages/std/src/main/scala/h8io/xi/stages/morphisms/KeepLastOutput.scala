package h8io.xi.stages.morphisms

import h8io.xi.stages.std.Fruitful
import h8io.xi.stages.{Morphism, Stage, Wrapper, Yield}

object KeepLastOutput {
  private[morphisms] final case class None[-I, +O, +E](stage: Stage[I, O, E]) extends Wrapper.Endo[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      stage(in) match {
        case Yield.Some(out, signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
        case Yield.None(signal, onDone) => Yield.None(signal, onDone.map(None(_)))
      }
  }

  private[morphisms] final case class Some[-I, +O, +E](out: O, stage: Stage[I, O, E])
      extends Wrapper.Endo[I, O, E] with Fruitful[I, O, E] {
    def apply(in: I): Yield.Some[I, O, E] =
      stage(in) match {
        case Yield.Some(out, signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
        case Yield.None(signal, onDone) => Yield.Some(out, signal, onDone.map(Some(out, _)))
      }
  }

  def apply[I, O, E](stage: Stage[I, O, E]): Wrapper.Endo[I, O, E] = None(stage)

  def morphism[I, O, E]: Morphism.Endo[I, O, E] = apply
}
