package h8io.xi.stages

import cats.data.NonEmptyChain

sealed trait Signal[+E] {
  private[stages] def compose[_E >: E](next: Signal[_E]): Signal[_E]

  private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E]

  private[stages] def break: Signal[E]
}

object Signal {
  case object Success extends Signal[Nothing] {
    private[stages] def compose[E](next: Signal[E]): Signal[E] =
      next match {
        case Success => this
        case that => that
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onSuccess()

    private[stages] def break: Signal[Nothing] = Complete
  }

  sealed trait Break[+E] extends Signal[E] {
    private[stages] def break: Signal[E] = this
  }

  case object Complete extends Break[Nothing] {
    private[stages] def compose[E](next: Signal[E]): Signal[E] =
      next match {
        case Success | Complete => this
        case that => that
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onComplete()
  }

  final case class Error[+E](causes: NonEmptyChain[E]) extends Break[E] {
    private[stages] def compose[_E >: E](next: Signal[_E]): Signal[_E] =
      next match {
        case Success | Complete => this
        case Error(nextCauses) => Error(causes ++ nextCauses)
      }

    private[stages] def apply[I, O, _E](onDone: OnDone[I, O, _E]): Stage[I, O, _E] = onDone.onError()
  }

  def error[E](error: E): Signal.Error[E] = Error(NonEmptyChain.one(error))
}
