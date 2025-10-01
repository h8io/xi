package h8io.xi.stages

trait Yield[-I, +O, +E] {
  val state: State[E]
  val onDone: OnDone[I, O, E]
}

object Yield {
  final case class Some[-I, +O, +E](out: O, state: State[E], onDone: OnDone[I, O, E]) extends Yield[I, O, E]

  final case class None[-I, +O, +E](state: State[E], onDone: OnDone[I, O, E]) extends Yield[I, O, E]
}
