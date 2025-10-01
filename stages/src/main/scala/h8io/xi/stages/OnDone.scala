package h8io.xi.stages

trait OnDone[-I, +O, +E] extends (() => Stage[I, O, E]) {
  def onSuccess(): Stage[I, O, E] = apply()
  def onComplete(): Stage[I, O, E] = apply()
  def onError(): Stage[I, O, E] = apply()

  def onPanic(): Unit = {}

  def apply(): Stage[I, O, E]
}

object OnDone {}
