package h8io.xi.stages

sealed trait Yield[-I, +O, +E] {
  val state: State[E]
  val onDone: OnDone[I, O, E]

  private[stages] def map[_I, _O, _E](
      mapOut: O => _O,
      mapState: State[E] => State[_E],
      mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield[_I, _O, _E]

  private[stages] def mapOnDone[_I, _O >: O, _E](
      state: State[_E],
      mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield[_I, _O, _E]

  private[stages] def mapOnDone[_I, _O >: O, _E >: E](
      mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield[_I, _O, _E]
}

object Yield {
  final case class Some[-I, +O, +E](out: O, state: State[E], onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](that: Yield[O, _O, _E]): Yield[I, _O, _E] =
      that match {
        case Yield.Some(out, state, onDone) => Yield.Some(out, this.state ~> state, this.onDone combine onDone)
        case Yield.None(state, onDone) => Yield.None(this.state ~> state, this.onDone combine onDone)
      }

    private[stages] def map[_I, _O, _E](
        mapOut: O => _O,
        mapState: State[E] => State[_E],
        mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield.Some[_I, _O, _E] =
      Yield.Some(mapOut(out), mapState(state), mapOnDone(onDone))

    private[stages] def mapOnDone[_I, _O >: O, _E](
        state: State[_E],
        mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield.Some[_I, _O, _E] =
      Yield.Some(out, state, mapOnDone(onDone))

    override private[stages] def mapOnDone[_I, _O >: O, _E >: E](
        mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield.Some[_I, _O, _E] =
      Yield.Some(out, state, mapOnDone(onDone))
  }

  final case class None[-I, +O, +E](state: State[E], onDone: OnDone[I, O, E]) extends Yield[I, O, E] {
    private[stages] def ~>[_O, _E >: E](next: Stage[O, _O, _E]): Yield.None[I, _O, _E] =
      Yield.None(state, onDone combine next)

    private[stages] def map[_I, _O, _E](
        mapOut: O => _O,
        mapState: State[E] => State[_E],
        mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield.None[_I, _O, _E] =
      Yield.None(mapState(state), mapOnDone(onDone))

    private[stages] def mapOnDone[_I, _O >: O, _E](
        state: State[_E],
        mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield.None[_I, _O, _E] = Yield.None(state, mapOnDone(onDone))

    override private[stages] def mapOnDone[_I, _O >: O, _E >: E](
        mapOnDone: OnDone[I, O, E] => OnDone[_I, _O, _E]): Yield.None[_I, _O, _E] = Yield.None(state, mapOnDone(onDone))
  }
}
