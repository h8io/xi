package h8io.xi.stages.util
import h8io.xi.stages.{OnDone, Signal, Yield}

trait Fn[-I, +O] extends Fruitful[I, O, Nothing] with OnDone.Static[I, O, Nothing] {
  def f(in: I): O

  final def apply(in: I): Yield.Some[I, O, Nothing] = Yield.Some(f(in), Signal.Success, this)
}
