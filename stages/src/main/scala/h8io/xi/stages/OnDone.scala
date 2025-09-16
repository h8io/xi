package h8io.xi.stages

trait OnDone[-I, +O, +E] {
  self =>

  def onSuccess(): State[I, O, E]
  def onComplete(): State[I, O, E]
  def onFailure(): State[I, O, E]

  private[stages] def safe: OnDone.Safe[I, O, E] = new OnDone.Safe[I, O, E] {
    override def onSuccess(): State[I, O, E] =
      try self.onSuccess()
      catch { case e: Exception => State.failure(e) }

    override def onComplete(): State[I, O, E] =
      try self.onComplete()
      catch { case e: Exception => State.failure(e) }

    override def onFailure(): State[I, O, E] =
      try self.onFailure()
      catch { case e: Exception => State.failure(e) }
  }
}

object OnDone {
  trait Safe[-I, +O, +E] extends OnDone[I, O, E] {
    self =>

    final private[stages] def <~[_O, _E >: E](that: OnDone.Safe[O, _O, _E]): OnDone.Safe[I, _O, _E] =
      new OnDone.Safe[I, _O, _E] {
        def onSuccess(): State[I, _O, _E] = that.onSuccess() ~> self
        def onComplete(): State[I, _O, _E] = that.onComplete() ~> self
        def onFailure(): State[I, _O, _E] = that.onFailure() ~> self
      }

    final override private[stages] def safe: Safe[I, O, E] = this
  }

  // Only one method of OnDone should be called, so no need to memoize state
  private[stages] def of[I, O, E](state: => State[I, O, E]): OnDone[I, O, E] = new OnDone[I, O, E] {
    override def onSuccess(): State[I, O, E] = state
    override def onComplete(): State[I, O, E] = state
    override def onFailure(): State[I, O, E] = state
  }

  def DoNothing[I, O, E](stage: Stage[I, O, E]): OnDone[I, O, E] = of(State.Success(stage))

  def OnFailure[E](e: Exception): OnDone[Any, Nothing, E] = of(State.failure(e))
}
