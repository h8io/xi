package h8io.xi.stages.morphisms

import h8io.xi.stages.morphisms.LocalSoftDeadline._OnDone
import h8io.xi.stages.std.DeadEnd
import h8io.xi.stages.*

import scala.concurrent.duration.FiniteDuration

final case class LocalSoftDeadline[-I, +O, +E](
    tsSupplier: () => Long,
    now: () => Long,
    duration: Long,
    stage: Stage[I, O, E])
    extends Wrapper.Endo[I, O, E] {
  def apply(in: I): Yield[I, O, E] = {
    val ts = tsSupplier()
    val yld = stage(in)
    if (now() - ts >= duration) yld.mapOnDoneAndBreak(_.map(LocalSoftDeadline(now, now, duration, _)))
    else yld.mapOnDone(_OnDone(() => ts, now, duration, _))
  }
}

object LocalSoftDeadline {
  private[morphisms] final case class _OnDone[-I, +O, +E](
      ts: () => Long,
      now: () => Long,
      duration: Long,
      onDone: OnDone[I, O, E])
      extends OnDone[I, O, E] {
    def onSuccess(): Stage[I, O, E] = LocalSoftDeadline(ts, now, duration, onDone.onSuccess())
    def onComplete(): Stage[I, O, E] = LocalSoftDeadline(now, now, duration, onDone.onComplete())
    def onError(): Stage[I, O, E] = LocalSoftDeadline(now, now, duration, onDone.onError())
  }

  def apply[I, O, E](duration: FiniteDuration, stage: Stage[I, O, E]): Stage[I, O, E] = apply(duration.toNanos, stage)

  def apply[I, O, E](duration: java.time.Duration, stage: Stage[I, O, E]): Stage[I, O, E] =
    apply(duration.toNanos, stage)

  @inline private def apply[I, O, E](duration: Long, stage: Stage[I, O, E]): Stage[I, O, E] =
    if (duration > 0) LocalSoftDeadline(now, now, duration, stage) else DeadEnd

  private val now: () => Long = System.nanoTime _

  def morphism[I, O, E](duration: FiniteDuration): Morphism.Endo[I, O, E] = apply(duration, _)

  def morphism[I, O, E](duration: java.time.Duration): Morphism.Endo[I, O, E] = apply(duration, _)
}
