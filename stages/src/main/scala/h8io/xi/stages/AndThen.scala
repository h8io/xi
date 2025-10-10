package h8io.xi.stages

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
