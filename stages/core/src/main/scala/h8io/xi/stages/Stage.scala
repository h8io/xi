package h8io.xi.stages

trait Stage[-I, +O, +E] extends (I => Yield[I, O, E]) {
  def apply(in: I): Yield[I, O, E]

  @inline final def ~>[_O, _E >: E](that: Stage[O, _O, _E]): Stage[I, _O, _E] = AndThen(this, that)

  @inline final def <~[_I, _E >: E](that: Stage[_I, I, _E]): Stage[_I, O, _E] = that ~> this

  @inline final def ~>[S <: Stage[?, ?, ?], _O, _E >: E](
      morphism: Morphism[S, Stage[O, _O, _E]]): Morphism[S, Stage[I, _O, _E]] = stage => this ~> morphism(stage)

  @inline final def |>[S <: Stage[?, ?, ?]](morphism: Morphism[Stage[I, O, E], S]): S = morphism ⋅ this

  def dispose(): Unit = {}
}

object Stage {
  type Endo[T, +E] = Stage[T, T, E]
}
