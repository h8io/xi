package h8io.xi.stages.std

import h8io.xi.stages.{OnDone, Signal, Stage, Yield}

object Unlift extends Stage[Option[Any], Any, Nothing] with OnDone.Static[Option[Any], Any, Nothing] {
  override def apply(in: Option[Any]): Yield[Option[Any], Any, Nothing] =
    in match {
      case Some(out) => Yield.Some(out, Signal.Success, this)
      case None => Yield.None(Signal.Success, this)
    }

  def apply[T]: Stage[Option[T], T, Nothing] = asInstanceOf[Stage[Option[T], T, Nothing]]
}
