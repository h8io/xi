package h8io.xi.stages

trait OnDone[-I, +O, +E] {
  self =>

  def onSuccess(): State[I, O, E]
  def onComplete(): State[I, O, E]
  def onFailure(): State[I, O, E]
  def dispose(): Unit

  private[stages] def safe: OnDone.Safe[I, O, E] = new OnDone.Safe[I, O, E] {
    def onSuccess(): State[I, O, E] =
      try self.onSuccess()
      catch { case e: Exception => State.panic(e) }

    def onComplete(): State[I, O, E] =
      try self.onComplete()
      catch { case e: Exception => State.panic(e) }

    def onFailure(): State[I, O, E] =
      try self.onFailure()
      catch { case e: Exception => State.panic(e) }

    def dispose(): Unit = self.dispose()
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
        def dispose(): Unit = {
          that.dispose()
          self.dispose()
        }
      }

    final override private[stages] def safe: Safe[I, O, E] = this
  }
}
