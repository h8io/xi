package h8io.xi.stages

@FunctionalInterface
trait Stage[-I, +O, +E] extends (I => Yield[I, O, E]) {
  def apply(in: I): Yield[I, O, E]

  @inline final def ~>[_O, _E >: E](that: Stage[O, _O, _E]): Stage[I, _O, _E] = Stage.AndThen(this, that)

  @inline final def <~[_I, _E >: E](that: Stage[_I, I, _E]): Stage[_I, O, _E] = that ~> this

  @inline final def ~>[S <: Stage[?, ?, ?], _O, _E >: E](
      alteration: Alteration[S, Stage[O, _O, _E]]): Alteration[S, Stage[I, _O, _E]] = stage => this ~> alteration(stage)

  @inline final def |>[S <: Stage[?, ?, ?]](alteration: Alteration[Stage[I, O, E], S]): S = alteration ⋅ this

  def dispose(): Unit = {}
}

object Stage {
  type Endo[T, +E] = Stage[T, T, E]

  final case class AndThen[-I, OI, +O, +E](previous: Stage[I, OI, E], next: Stage[OI, O, E]) extends Stage[I, O, E] {
    def apply(in: I): Yield[I, O, E] =
      previous(in) match {
        case some @ Yield.Some(out, _, _) => some ~> next(out)
        case none: Yield.None[I, OI, E] => none ~> next
      }

    override def dispose(): Unit = {
      next.dispose()
      previous.dispose()
    }
  }
}
