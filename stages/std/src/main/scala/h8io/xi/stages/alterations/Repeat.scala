package h8io.xi.stages.alterations

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Wrapper.Deco[I, O, E] {
  def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stage: Stage[I, O, E]): Yield[I, O, E] = {
      val yld = stage(in)
      yld.signal match {
        case Signal.Success => repeat(yld.onDone.onSuccess())
        case Signal.Complete =>
          yld.mapOnDone(Signal.Success, onDone => Repeat(onDone.onComplete()))
        case error: Signal.Error[E] => yld.mapOnDone(error, onDone => Repeat(onDone.onError()))
      }
    }
    repeat(stage)
  }
}
