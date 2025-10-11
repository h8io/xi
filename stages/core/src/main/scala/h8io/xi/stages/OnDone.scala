package h8io.xi.stages

trait OnDone[-I, +O, +E] {
  self =>

  def onSuccess(): Stage[I, O, E]
  def onComplete(): Stage[I, O, E]
  def onError(): Stage[I, O, E]

  @inline private[stages] final def combine[_O, _E >: E](that: OnDone[O, _O, _E]): OnDone[I, _O, _E] =
    new OnDone[I, _O, _E] {
      def onSuccess(): Stage[I, _O, _E] = that.onSuccess() <~ self.onSuccess()
      def onComplete(): Stage[I, _O, _E] = that.onComplete() <~ self.onComplete()
      def onError(): Stage[I, _O, _E] = that.onError() <~ self.onError()
    }

  @inline private[stages] final def combine[_O, _E >: E](stage: Stage[O, _O, _E]): OnDone[I, _O, _E] =
    new OnDone[I, _O, _E] {
      def onSuccess(): Stage[I, _O, _E] = self.onSuccess() ~> stage
      def onComplete(): Stage[I, _O, _E] = self.onComplete() ~> stage
      def onError(): Stage[I, _O, _E] = self.onError() ~> stage
    }

  final def map[_I, _O, _E](f: Stage[I, O, E] => Stage[_I, _O, _E]): OnDone[_I, _O, _E] =
    new OnDone[_I, _O, _E] {
      def onSuccess(): Stage[_I, _O, _E] = f(self.onSuccess())
      def onComplete(): Stage[_I, _O, _E] = f(self.onComplete())
      def onError(): Stage[_I, _O, _E] = f(self.onError())
    }
}

object OnDone {
  final case class FromStage[-I, +O, +E](stage: Stage[I, O, E]) extends OnDone[I, O, E] {
    def onSuccess(): Stage[I, O, E] = stage
    def onComplete(): Stage[I, O, E] = stage
    def onError(): Stage[I, O, E] = stage
  }

  trait Static[-I, +O, +E] extends OnDone[I, O, E] {
    self: Stage[I, O, E] =>

    def onComplete(): Stage[I, O, E] = self
    def onSuccess(): Stage[I, O, E] = self
    def onError(): Stage[I, O, E] = self
  }
}
