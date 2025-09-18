package h8io.xi.stages

sealed trait Outcome[-I, +O, +E] {
  val state: State[I, O, E]
  val dispose: () => Unit
}

object Outcome {
  final case class Some[-I, +O, +E](out: O, state: State[I, O, E], dispose: () => Unit) extends Outcome[I, O, E]
  final case class None[-I, +O, +E](state: State[I, O, E], dispose: () => Unit) extends Outcome[I, O, E]
}
