package h8io.xi.stages.alterators

import h8io.xi.stages.std.Fruitful
import h8io.xi.stages.{Stage, Wrapper, Yield}

final case class Lift[I, O, E](stage: Stage[I, O, E])
    extends Wrapper[Stage[I, O, E], I, scala.Option[O], E] with Fruitful[I, scala.Option[O], E] {
  def apply(in: I): Yield.Some[I, scala.Option[O], E] =
    stage(in) match {
      case Yield.Some(out, signal, onDone) => Yield.Some(Some(out), signal, onDone.map(Lift(_)))
      case Yield.None(signal, onDone) => Yield.Some(None, signal, onDone.map(Lift(_)))
    }
}
