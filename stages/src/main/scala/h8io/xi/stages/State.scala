package h8io.xi.stages

import cats.data.NonEmptyChain

sealed trait State[+E] {
  private[stages] def ~>[_E >: E](next: State[_E]): State[_E]

  private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E]
}

object State {
  case object Success extends State[Nothing] {
    private[stages] def ~>[E](next: State[E]): State[E] =
      next match {
        case Success => this
        case that => that
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onSuccess()
  }

  case object Complete extends State[Nothing] {
    private[stages] def ~>[E](next: State[E]): State[E] =
      next match {
        case Success | Complete => this
        case that => that
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onComplete()
  }

  final case class Error[E](causes: NonEmptyChain[E]) extends State[E] {
    private[stages] def ~>[_E >: E](next: State[_E]): State[_E] =
      next match {
        case Success | Complete => this
        case Error(nextCauses) => Error(causes ++ nextCauses)
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onError()
  }
}
