package h8io.xi.stages

final case class Loop[I, +E](stage: Stage[I, I, E]) extends Stage[I, I, E] {
  def apply(in: I): Yield[I, I, E] = ???
}
