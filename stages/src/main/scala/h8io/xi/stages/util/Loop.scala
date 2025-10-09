package h8io.xi.stages.util

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Loop[T, +E](stage: Stage.Endo[T, E]) extends Decorator[T, T, E] {
  def apply(in: T): Yield[T, T, E] = {
    @tailrec def loop(stage: Stage[T, T, E], in: T): Yield[T, T, E] = {
      val `yield` = stage(in)
      `yield`.signal match {
        case Signal.Success =>
          `yield` match {
            case Yield.Some(out, _, _) => loop(`yield`.onDone.onSuccess(), out)
            case Yield.None(_, _) => Yield.None(Signal.Success, OnDone.FromStage(Loop(`yield`.onDone.onComplete())))
          }
        case Signal.Complete => `yield`.mapOnDone(Signal.Success, onDone => OnDone.FromStage(Loop(onDone.onComplete())))
        case error: Signal.Error[E] => `yield`.mapOnDone(error, onDone => OnDone.FromStage(Loop(onDone.onError())))
      }
    }
    loop(stage, in)
  }
}
