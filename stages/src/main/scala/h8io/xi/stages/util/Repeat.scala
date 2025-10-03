package h8io.xi.stages.util

import h8io.xi.stages.*

import scala.annotation.tailrec

final case class Repeat[-I, +O, +E](stage: Stage[I, O, E]) extends Stage[I, O, E] {
  def apply(in: I): Yield[I, O, E] = {
    @tailrec def repeat(stage: Stage[I, O, E]): Yield[I, O, E] = {
      val `yield` = stage(in)
      `yield`.state match {
        case State.Success => repeat(`yield`.onDone.onSuccess())
        case State.Complete => `yield`.mapOnDone(State.Success, onDone => OnDone.FromStage(Repeat(onDone.onComplete())))
        case error: State.Error[E] => `yield`.mapOnDone(error, onDone => OnDone.FromStage(Repeat(onDone.onError())))
      }
    }
    repeat(stage)
  }

  override def dispose(): Unit = stage.dispose()
}
