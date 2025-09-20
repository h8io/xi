package h8io.xi.stages

sealed trait Yield[-I, +O, +E] {
  val onDone: OnDone[I, O, E]

  private[stages] def ~>[_O, _E >: E](stage: Stage[O, _O, _E]): Yield[I, _O, _E]

  private[stages] def lift[_I, _O >: O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Yield[_I, _O, _E]

  private[stages] def complete[_I, _O >: O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Yield[_I, _O, _E]

  private[stages] def outcome: Outcome[I, O, E]
}

object Yield {
  final case class Some[-I, +O, +E](out: O, onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](stage: Stage[O, _O, _E]): Yield[I, _O, _E] = stage.safe(out) match {
      case Some(_out, _onDone) => Some(_out, onDone.safe <~ _onDone.safe)
      case None(_onDone) => None(onDone.safe <~ _onDone.safe)
    }

    private[stages] def lift[_I, _O >: O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Some[_I, _O, _E] =
      Some(out, onDone.lift(f))

    private[stages] def complete[_I, _O >: O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Some[_I, _O, _E] =
      Some(out, onDone.complete(f))

    private[stages] def outcome: Outcome.Some[I, O, E] = {
      val safeOnDone = onDone.safe
      Outcome.Some(out, safeOnDone.onSuccess(), safeOnDone.dispose _)
    }
  }

  final case class None[-I, +O, +E](onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](stage: Stage[O, _O, _E]): Yield.None[I, _O, _E] = {
      Yield.None(new OnDone[I, _O, _E] {
        def onSuccess(): State[I, _O, _E] = onDone.onSuccess() <~ State.Success(stage)
        def onComplete(): State[I, _O, _E] = onDone.onComplete() <~ State.Success(stage)
        def onError(): State[I, _O, _E] = onDone.onError() <~ State.Success(stage)
        def onPanic(): State[I, _O, _E] = onDone.onPanic() <~ State.Success(stage)
        def dispose(): Unit = onDone.dispose()
      })
    }

    private[stages] def lift[_I, _O >: O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): None[_I, _O, _E] =
      None(onDone.lift(f))

    private[stages] def complete[_I, _O >: O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): None[_I, _O, _E] =
      None(onDone.complete(f))

    private[stages] def outcome: Outcome.None[I, O, E] = {
      val safeOnDone = onDone.safe
      Outcome.None(safeOnDone.safe.onSuccess(), safeOnDone.dispose _)
    }
  }
}
