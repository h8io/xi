package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

final case class Cache[-I, +O, +E](stage: Stage[I, O, E]) extends Stage.Decorator[I, O, E] {
  def apply(in: I): Yield[I, O, E] =
    stage(in) match {
      case Yield.Some(out, Signal.Success, onDone) =>
        Yield.Some(
          out,
          Signal.Success,
          new OnDone[I, O, E] {
            def onSuccess(): Stage[I, O, E] = Cache.Cached(out, onDone.onSuccess())
            def onComplete(): Stage[I, O, E] = Cache(onDone.onComplete())
            def onError(): Stage[I, O, E] = Cache(onDone.onError())
          }
        )
      case yld => yld.mapOnDone(_.map(Cache(_)))
    }
}

object Cache {
  private[util] final case class Cached[-I, +O, +E](out: O, stage: Stage[I, O, E])
      extends Stage.Decorator[I, O, E] with OnDone[I, O, E] {
    def apply(in: I): Yield[I, O, E] = Yield.Some(out, Signal.Success, this)

    def onSuccess(): Stage[I, O, E] = this
    def onComplete(): Stage[I, O, E] = Cache(stage)
    def onError(): Stage[I, O, E] = Cache(stage)
  }
}
