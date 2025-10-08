package h8io.xi.stages.util

import h8io.xi.stages
import h8io.xi.stages.{Stage, Signal, Yield}

final case class Const[+O](out: O) extends Stage[Any, O, Nothing] {
  self =>

  def apply(in: Any): Yield[Any, O, Nothing] = Yield.Some(out, Signal.Success, OnDone)

  private[util] case object OnDone extends stages.OnDone[Any, O, Nothing] {
    def onSuccess(): Stage[Any, O, Nothing] = self
    def onComplete(): Stage[Any, O, Nothing] = self
    def onError(): Stage[Any, O, Nothing] = self
  }
}
