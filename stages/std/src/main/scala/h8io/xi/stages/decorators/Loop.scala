package h8io.xi.stages.decorators

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Loop[T, +E](stage: Stage.Endo[T, E]) extends Decorated.Endo[T, T, E] {
  def apply(in: T): Yield[T, T, E] = {
    @tailrec def loop(stage: Stage[T, T, E], in: T): Yield[T, T, E] = {
      val yld = stage(in)
      yld.signal match {
        case Signal.Success =>
          yld match {
            case Yield.Some(out, _, _) => loop(yld.onDone.onSuccess(), out)
            case Yield.None(_, _) => Yield.None(Signal.Success, OnDone.FromStage(Loop(yld.onDone.onComplete())))
          }
        case Signal.Complete => yld.mapOnDone(Signal.Success, onDone => OnDone.FromStage(Loop(onDone.onComplete())))
        case error: Signal.Error[E] => yld.mapOnDone(error, onDone => OnDone.FromStage(Loop(onDone.onError())))
      }
    }
    loop(stage, in)
  }
}
