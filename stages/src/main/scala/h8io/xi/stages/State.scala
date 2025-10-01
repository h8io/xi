package h8io.xi.stages

import cats.data.NonEmptyChain

sealed trait State[+E] {
  private[stages] def ~>[_E >: E](next: State[_E]): State[_E]
}

object State {
  case object Success extends State[Nothing] {
    private[stages] def ~>[E](next: State[E]): State[E] =
      next match {
        case Success => this
        case that => that
      }
  }

  case object Complete extends State[Nothing] {
    private[stages] def ~>[E](next: State[E]): State[E] =
      next match {
        case Success | Complete => this
        case that => that
      }
  }

  final case class Error[E](errors: NonEmptyChain[E]) extends State[E] {
    override private[stages] def ~>[_E >: E](next: State[_E]): State[_E] =
      next match {
        case Success | Complete => this
        case Error(nextErrors) => Error(errors ++ nextErrors)
        case that => that
      }
  }
}
