package h8io.xi.stages

import cats.data.NonEmptyChain

import scala.annotation.nowarn

sealed trait State[-I, +O, +E] {
  private[stages] def <~[_O, _E >: E](that: State[O, _O, _E]): State[I, _O, _E]

  private[stages] def ~>[_I, _E >: E](onDone: OnDone[_I, I, _E]): State[_I, O, _E]
}

object State {
  final case class Success[-I, +O, +E](lazyStage: () => Stage[I, O, E]) extends State[I, O, E] {
    private[stages] def <~[_O, _E >: E](that: State[O, _O, _E]): State[I, _O, _E] = that match {
      case Success(nextStage) => Success(() => lazyStage() ~> nextStage())
      case Complete => Complete
      case failure: Failure[?] => failure
    }

    private[stages] def ~>[_I, _E >: E](onDone: OnDone[_I, I, _E]): State[_I, O, _E] = onDone.onSuccess() <~ this
  }

  final case object Complete extends State[Any, Nothing, Nothing] {
    private[stages] def <~[_O, _E](state: State[Nothing, _O, _E]): State[Any, Nothing, _E] = state match {
      case failure: Failure[_] => failure
      case _ => Complete
    }

    private[stages] def ~>[_I, _E](onDone: OnDone[_I, Any, _E]): State[_I, Nothing, _E] = onDone.onComplete() <~ this
  }

  @nowarn("msg=access modifiers for `copy` method are copied from the case class constructor under Scala 3")
  @nowarn("msg=access modifiers for `apply` method are copied from the case class constructor under Scala 3")
  final case class Failure[+E] private (failures: NonEmptyChain[Either[Exception, E]]) extends State[Any, Nothing, E] {
    private[stages] def <~[_O, _E >: E](state: State[Nothing, _O, _E]): Failure[_E] = state match {
      case Failure(previous) => Failure(previous ++ failures)
      case _ => this
    }

    private[stages] def ~>[_I, _E >: E](onDone: OnDone[_I, Any, _E]): State[_I, Nothing, _E] =
      onDone.onFailure() <~ this
  }

  def error[E](head: E, tail: E*): Failure[E] = Failure(NonEmptyChain(Right(head), (tail map Right.apply)*))

  private[stages] def failure(e: Exception): Failure[Nothing] = Failure(NonEmptyChain(Left(e)))
}
