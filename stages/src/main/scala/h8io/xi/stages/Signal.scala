package h8io.xi.stages

import cats.data.NonEmptyChain

sealed trait Signal[+E] {
  private[stages] def ~>[_E >: E](next: Signal[_E]): Signal[_E]

  private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E]
}

object Signal {
  case object Success extends Signal[Nothing] {
    private[stages] def ~>[E](next: Signal[E]): Signal[E] =
      next match {
        case Success => this
        case that => that
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onSuccess()
  }

  case object Complete extends Signal[Nothing] {
    private[stages] def ~>[E](next: Signal[E]): Signal[E] =
      next match {
        case Success | Complete => this
        case that => that
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onComplete()
  }

  final case class Error[E](causes: NonEmptyChain[E]) extends Signal[E] {
    private[stages] def ~>[_E >: E](next: Signal[_E]): Signal[_E] =
      next match {
        case Success | Complete => this
        case Error(nextCauses) => Error(causes ++ nextCauses)
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onError()
  }

  def error[E](error: E): Signal.Error[E] = Error(NonEmptyChain.one(error))
}
