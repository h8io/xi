package h8io.xi.stages.morphisms

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Wrapper.Endo[I, O, E] {
  def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stage: Stage[I, O, E]): Yield[I, O, E] = {
      val yld = stage(in)
      yld.signal match {
        case Signal.Success => repeat(yld.onDone.onSuccess())
        case Signal.Complete =>
          yld.mapOnDone(Signal.Success, onDone => OnDone.FromStage(Repeat(onDone.onComplete())))
        case error: Signal.Error[E] => yld.mapOnDone(error, onDone => OnDone.FromStage(Repeat(onDone.onError())))
      }
    }
    repeat(stage)
  }
}

object Repeat {
  def morphism[I, O, E]: Morphism.Endo[I, O, E] = apply
}
