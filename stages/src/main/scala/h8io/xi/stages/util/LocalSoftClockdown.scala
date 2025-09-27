package h8io.xi.stages.util

import h8io.xi.stages.{OnDone, Stage, State, Yield}

import scala.concurrent.duration.FiniteDuration

object LocalSoftClockdown {
  private[util] final case class Head[-I, +O, +E](now: () => Long, duration: Long, stage: Stage[I, O, E])
      extends Stage.Safe[I, O, E] {
    assume(duration > 0, s"Duration must be positive, got duration = $duration")

    def apply(in: I): Yield[I, O, E] = {
      val `yield` = stage.safe(in)
      `yield`.lift(Tail(now(), now, duration, `yield`.onDone.dispose _, _))
    }
  }

  private[util] final case class Tail[-I, +O, +E](
      ts: Long,
      now: () => Long,
      duration: Long,
      dispose: () => Unit,
      stage: Stage[I, O, E]
  ) extends Stage.Safe[I, O, E] {
    self =>

    assume(duration > 0, s"Duration value should be positive: $duration")

    override def apply(in: I): Yield[I, O, E] =
      if (overdue()) Yield.None(State.Complete(Head(now, duration, stage)).onDone(dispose))
      else stage.safe(in) map { onDone =>
        new OnDone[I, O, E] {
          def onSuccess(): State[I, O, E] = {
            val state = onDone.onSuccess()
            if (overdue()) state.complete(Head(now, duration, _))
            else state.map(Tail(ts, now, duration, onDone.dispose _, _))
          }

          def onComplete(): State[I, O, E] = onDone.onComplete().complete(Head(now, duration, _))
          def onError(): State[I, O, E] = onDone.onError().complete(Head(now, duration, _))
          def onPanic(): State[I, O, E] = onDone.onPanic().complete(Head(now, duration, _))

          override def dispose(): Unit = onDone.dispose()
        }
      }

    @inline private def overdue(): Boolean = now() - ts >= duration
  }

  def apply[I, O, E](duration: FiniteDuration, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    apply(duration.toNanos, stage)

  def apply[I, O, E](duration: java.time.Duration, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    apply(duration.toNanos, stage)

  @inline private def apply[I, O, E](duration: Long, stage: Stage[I, O, E]): Stage.Safe[I, O, E] =
    if (duration > 0) Head(System.nanoTime _, duration, stage) else DeadEnd
}
