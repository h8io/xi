package h8io.xi.stages

trait Stage[-I, +O, +E] extends (I => Yield[I, O, E]) {
  self =>

  def apply(in: I): Yield[I, O, E]

  @inline final def ~>[_O, _E >: E](that: Stage[O, _O, _E]): Stage[I, _O, _E] = Stage.AndThen(this, that)

  @inline final def <~[_I, _E >: E](that: Stage[_I, I, _E]): Stage[_I, O, _E] = that ~> this

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

    override def toString(): String = s"$previous ~> $next"
  }
}
