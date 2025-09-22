package h8io.xi.stages

trait OnDone[-I, +O, +E] {
  self =>

  def onSuccess(): State[I, O, E]
  def onComplete(): State[I, O, E]
  def onError(): State[I, O, E]
  def onPanic(): State[I, O, E]

  def dispose(): Unit = {}

  private[stages] def safe: OnDone.Safe[I, O, E] =
    new OnDone.Safe[I, O, E] {
      def onSuccess(): State[I, O, E] =
        try self.onSuccess()
        catch { case e: Exception => State.Panic(e) }

      def onComplete(): State[I, O, E] =
        try self.onComplete()
        catch { case e: Exception => State.Panic(e) }

      def onError(): State[I, O, E] =
        try self.onError()
        catch { case e: Exception => State.Panic(e) }

      def onPanic(): State[I, O, E] =
        try self.onPanic()
        catch { case e: Exception => State.Panic(e) }

      override def dispose(): Unit = self.dispose()
    }

  final def map[_I, _O, _E](f: State[I, O, E] => State[_I, _O, _E]): OnDone[_I, _O, _E] =
    new OnDone[_I, _O, _E] {
      def onSuccess(): State[_I, _O, _E] = f(self.onSuccess())
      def onComplete(): State[_I, _O, _E] = f(self.onComplete())
      def onError(): State[_I, _O, _E] = f(self.onError())
      def onPanic(): State[_I, _O, _E] = f(self.onPanic())

      override def dispose(): Unit = self.dispose()
    }

  final def lift[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): OnDone[_I, _O, _E] = map(_.map(f))

  final def complete[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): OnDone[_I, _O, _E] =
    new OnDone[_I, _O, _E] {
      def onSuccess(): State[_I, _O, _E] = self.onComplete().complete(f)
      def onComplete(): State[_I, _O, _E] = self.onComplete().complete(f)
      def onError(): State[_I, _O, _E] = self.onError().complete(f)
      def onPanic(): State[_I, _O, _E] = self.onPanic().complete(f)

      override def dispose(): Unit = self.dispose()
    }
}

object OnDone {
  trait Safe[-I, +O, +E] extends OnDone[I, O, E] {
    self =>

    final private[stages] def <~[_O, _E >: E](that: Safe[O, _O, _E]): Safe[I, _O, _E] =
      new Safe[I, _O, _E] {
        def onSuccess(): State[I, _O, _E] = that.onSuccess() ~> self
        def onComplete(): State[I, _O, _E] = that.onComplete() ~> self
        def onError(): State[I, _O, _E] = that.onError() ~> self
        def onPanic(): State[I, _O, _E] = that.onPanic() ~> self

        override def dispose(): Unit = {
          that.dispose()
          self.dispose()
        }
      }

    final override private[stages] def safe: Safe[I, O, E] = this

    final def safeMap[_I, _O, _E](f: State[I, O, E] => State[_I, _O, _E]): OnDone.Safe[_I, _O, _E] =
      new OnDone.Safe[_I, _O, _E] {
        def onSuccess(): State[_I, _O, _E] = f(self.onSuccess())
        def onComplete(): State[_I, _O, _E] = f(self.onComplete())
        def onError(): State[_I, _O, _E] = f(self.onError())
        def onPanic(): State[_I, _O, _E] = f(self.onPanic())

        override def dispose(): Unit = self.dispose()
      }

    final def safeLift[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): OnDone.Safe[_I, _O, _E] =
      safeMap(_.map(f))

    final def safeComplete[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): OnDone.Safe[_I, _O, _E] =
      new OnDone.Safe[_I, _O, _E] {
        def onSuccess(): State[_I, _O, _E] = self.onComplete().complete(f)
        def onComplete(): State[_I, _O, _E] = self.onComplete().complete(f)
        def onError(): State[_I, _O, _E] = self.onError().complete(f)
        def onPanic(): State[_I, _O, _E] = self.onPanic().complete(f)

        override def dispose(): Unit = self.dispose()
      }
  }
}
