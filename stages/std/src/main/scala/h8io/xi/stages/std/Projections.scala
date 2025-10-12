package h8io.xi.stages.std

import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

object Projections {
  trait Left[P[_, _]] extends Any {
    def leftYield[L](in: P[L, ?], onDone: OnDone[P[L, ?], L, Nothing]): Yield[P[L, ?], L, Nothing]
  }

  trait Right[P[_, _]] extends Any {
    def rightYield[R](in: P[?, R], onDone: OnDone[P[?, R], R, Nothing]): Yield[P[?, R], R, Nothing]
  }

  trait Both[P[_, _]] extends Left[P] with Right[P]

  implicit object TupleProjections extends AnyRef with Both[Tuple2] {
    def leftYield[L](in: (L, ?), onDone: OnDone[(L, ?), L, Nothing]): Yield[(L, ?), L, Nothing] =
      Yield.Some(in._1, Signal.Success, onDone)

    def rightYield[R](in: (?, R), onDone: OnDone[(?, R), R, Nothing]): Yield[(?, R), R, Nothing] =
      Yield.Some(in._2, Signal.Success, onDone)
  }
}

final class Left[C[_, _], O](implicit lp: Projections.Left[C])
    extends Stage[C[O, ?], O, Nothing] with OnDone.Static[C[O, ?], O, Nothing] {
  def apply(in: C[O, ?]): Yield[C[O, ?], O, Nothing] = lp.leftYield(in, this)
}

object Left {
  def apply[C[_, _]: Projections.Left, O]: Left[C, O] = new Left[C, O]
}

final class Right[C[_, _], O](implicit rp: Projections.Right[C])
    extends Stage[C[?, O], O, Nothing] with OnDone.Static[C[?, O], O, Nothing] {
  def apply(in: C[?, O]): Yield[C[?, O], O, Nothing] = rp.rightYield(in, this)
}

object Right {
  def apply[C[_, _]: Projections.Right, O]: Right[C, O] = new Right[C, O]
}
