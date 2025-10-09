package h8io.xi.stages.util

import h8io.xi.stages.*

final case class Cache[-I, +O, +E](stage: Stage[I, O, E]) extends Decorator[I, O, E] {
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
      extends Decorator[I, O, E] with Stage.WithOnDone[I, O, E] {
    def apply(in: I): Yield[I, O, E] = Yield.Some(out, Signal.Success, this)

    override def onComplete(): Stage[I, O, E] = Cache(stage)
    override def onError(): Stage[I, O, E] = Cache(stage)
  }
}
