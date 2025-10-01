package h8io.xi.stages

trait Stage[-I, +O, +E] extends (I => Yield[I, O, E]) {
  def ~>[_O, _E >: E](that: Stage[O, _O, _E]): Stage[I, _O, _E]
}

object Stage {
  type Endo[-I, +O <: I, +E] = Stage[I, O, E]
}
