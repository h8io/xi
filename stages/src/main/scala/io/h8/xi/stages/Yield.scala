package io.h8.xi.stages

sealed trait Yield[-I, +O, +E] {
  val onDone: OnDone[I, O, E]

  private[stages] def ~>[_O, _E >: E](stage: Stage[O, _O, _E]): Yield[I, _O, _E]
}

object Yield {
  final case class Some[-I, +O, +E](out: O, onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](stage: Stage[O, _O, _E]): Yield[I, _O, _E] = stage.safe(out) match {
      case Some(_out, _onDone) => Some(_out, onDone.safe <~ _onDone.safe)
      case None(_onDone) => None(onDone.safe <~ _onDone.safe)
    }
  }

  final case class None[-I, +O, +E](onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](stage: Stage[O, _O, _E]): Yield.None[I, _O, _E] = {
      Yield.None(new OnDone[I, _O, _E] {
        override def onSuccess(): State[I, _O, _E] = onDone.onSuccess() <~ State.Success(() => stage)
        override def onComplete(): State[I, _O, _E] = onDone.onComplete() <~ State.Success(() => stage)
        override def onFailure(): State[I, _O, _E] = onDone.onFailure() <~ State.Success(() => stage)
      })
    }
  }
}
