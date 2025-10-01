package h8io.xi.stages

sealed trait Yield[-I, +O, +E] {
  val state: State[E]
  val onDone: OnDone[I, O, E]
}

object Yield {
  final case class Some[-I, +O, +E](out: O, state: State[E], onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](that: Yield[O, _O, _E]): Yield[I, _O, _E] =
      that match {
        case Yield.Some(out, state, onDone) => Yield.Some(out, this.state ~> state, this.onDone <~ onDone)
        case Yield.None(state, onDone) => Yield.None(this.state ~> state, this.onDone <~ onDone)
      }
  }

  final case class None[-I, +O, +E](state: State[E], onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](next: Stage[O, _O, _E]): Yield.None[I, _O, _E] =
      Yield.None(state, onDone ~> next)
  }
}
