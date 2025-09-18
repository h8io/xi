package h8io.xi.stages

sealed trait Outcome[-I, +O, +E] {
  val state: State[I, O, E]
  val dispose: () => Unit

  private[stages] def toYield[_I <: I, _O >: O, _E >: E](state: State[_I, _O, _E]): Yield[_I, _O, _E]
}

object Outcome {
  final case class Some[-I, +O, +E](out: O, state: State[I, O, E], dispose: () => Unit) extends Outcome[I, O, E] {
    private[stages] def toYield[_I <: I, _O >: O, _E >: E](state: State[_I, _O, _E]): Yield.Some[_I, _O, _E] =
      Yield.Some(out, state.onDone(dispose))
  }

  final case class None[-I, +O, +E](state: State[I, O, E], dispose: () => Unit) extends Outcome[I, O, E] {
    private[stages] def toYield[_I <: I, _O >: O, _E >: E](state: State[_I, _O, _E]): Yield.None[_I, _O, _E] =
      Yield.None(state.onDone(dispose))
  }
}
