package h8io.xi.stages

@FunctionalInterface
trait Stage[-I, +O, +E] extends (I => Yield[I, O, E]) with OnDone[I, O, E] {
  def apply(in: I): Yield[I, O, E]

  def dispose(): Unit = {}

  def onComplete(): Stage[I, O, E] = this
  def onSuccess(): Stage[I, O, E] = this
  def onError(): Stage[I, O, E] = this

  @inline final def outcome(in: I): Outcome[O, E] = this(in).outcome()

  @inline final def ~>[_O, _E >: E](that: Stage[O, _O, _E]): Stage[I, _O, _E] = Stage.AndThen(this, that)

  @inline final def <~[_I, _E >: E](that: Stage[_I, I, _E]): Stage[_I, O, _E] = that ~> this

  @inline final def ~>[S <: Stage.Any, _O, _E >: E](
      alterator: Alterator[S, Stage[O, _O, _E]]): Alterator[S, Stage[I, _O, _E]] = stage => this ~> alterator(stage)

  @inline final def |>[S <: Stage.Any](alterator: Alterator[Stage[I, O, E], S]): S = alterator ⋅ this

  @inline final def alterator[_O, _E >: E]: Alterator[Stage[O, _O, _E], Stage[I, _O, _E]] = leftAlterator[_O, _E]

  @inline final def leftAlterator[_O, _E >: E]: Alterator[Stage[O, _O, _E], Stage[I, _O, _E]] = ~>[_O, _E]

  @inline final def rightAlterator[_I, _E >: E]: Alterator[Stage[_I, I, _E], Stage[_I, O, _E]] = <~[_I, _E]
}

object Stage {
  type Endo[T, +E] = Stage[T, T, E]

  type Any = Stage[?, ?, ?]

  final case class AndThen[-I, OI, +O, +E](previous: Stage[I, OI, E], next: Stage[OI, O, E]) extends Stage[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      previous(in) match {
        case some @ Yield.Some(out, _, _) => some.compose(next(out))
        case none: Yield.None[I, OI, E] => none.compose(next)
      }

    override def dispose(): Unit = {
      next.dispose()
      previous.dispose()
    }
  }
}
