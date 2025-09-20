package h8io.xi.stages

import cats.data.NonEmptyChain

sealed trait State[-I, +O, +E] {
  self =>

  private[stages] def <~[_O, _E >: E](that: State[O, _O, _E]): State[I, _O, _E]

  private[stages] def ~>[_I, _E >: E](onDone: OnDone[_I, I, _E]): State[_I, O, _E]

  private[stages] def map[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): State[_I, _O, _E]

  private[stages] def complete[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): State[_I, _O, _E]

  final def onDone(_dispose: () => Unit = () => {}): OnDone.Safe[I, O, E] = new OnDone.Safe[I, O, E] {
    def onSuccess(): State[I, O, E] = self
    def onComplete(): State[I, O, E] = self
    def onError(): State[I, O, E] = self
    def onPanic(): State[I, O, E] = self

    def dispose(): Unit = _dispose()
  }
}

object State {
  final case class Success[-I, +O, +E](stage: Stage[I, O, E]) extends State[I, O, E] {
    private[stages] def <~[_O, _E >: E](that: State[O, _O, _E]): State[I, _O, _E] = that match {
      case panic: Panic => panic
      case Error(nextStage, errors) => Error(stage ~> nextStage, errors)
      case Complete(nextStage) => Complete(stage ~> nextStage)
      case Success(nextStage) => Success(stage ~> nextStage)
    }

    private[stages] def ~>[_I, _E >: E](onDone: OnDone[_I, I, _E]): State[_I, O, _E] = onDone.onSuccess() <~ this

    @inline
    private[stages] def map[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Success[_I, _O, _E] =
      Success(f(stage))

    private[stages] def complete[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Complete[_I, _O, _E] =
      Complete(f(stage))
  }

  final case class Complete[-I, +O, +E](stage: Stage[I, O, E]) extends State[I, O, E] {
    private[stages] def <~[_O, _E >: E](that: State[O, _O, _E]): State[I, _O, _E] = that match {
      case panic: Panic => panic
      case Error(nextStage, errors) => Error(stage ~> nextStage, errors)
      case Complete(nextStage) => Complete(stage ~> nextStage)
      case Success(nextStage) => Complete(stage ~> nextStage)
    }

    private[stages] def ~>[_I, _E >: E](onDone: OnDone[_I, I, _E]): State[_I, O, _E] = onDone.onComplete() <~ this

    private[stages] def map[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Complete[_I, _O, _E] =
      Complete(f(stage))

    private[stages] def complete[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Complete[_I, _O, _E] =
      map(f)
  }

  final case class Error[-I, +O, +E](stage: Stage[I, O, E], errors: NonEmptyChain[E]) extends State[I, O, E] {
    private[stages] def <~[_O, _E >: E](that: State[O, _O, _E]): State[I, _O, _E] = that match {
      case panic: Panic => panic
      case Error(nextStage, previousErrors) => Error(stage ~> nextStage, errors ++ previousErrors)
      case Complete(nextStage) => Error(stage ~> nextStage, errors)
      case Success(nextStage) => Error(stage ~> nextStage, errors)
    }

    private[stages] def ~>[_I, _E >: E](onDone: OnDone[_I, I, _E]): State[_I, O, _E] = onDone.onError() <~ this

    @inline
    private[stages] def map[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Error[_I, _O, _E] =
      Error(f(stage), errors)

    private[stages] def complete[_I, _O, _E >: E](f: Stage[I, O, E] => Stage[_I, _O, _E]): Error[_I, _O, _E] = map(f)
  }

  def Error[I, O, E](stage: Stage[I, O, E], error: E): Error[I, O, E] = Error(stage, NonEmptyChain.one(error))

  final case class Panic(exceptions: NonEmptyChain[Exception]) extends State[Any, Nothing, Nothing] {
    private[stages] def <~[_O, _E](that: State[Nothing, _O, _E]): State.Panic = that match {
      case Panic(previousExceptions) => Panic(previousExceptions ++ exceptions)
      case _ => this
    }

    private[stages] def ~>[_I, _E](onDone: OnDone[_I, Any, _E]): State[_I, Nothing, _E] = onDone.onPanic() <~ this

    private[stages] def map[_I, _O, _E](f: Stage[Any, Nothing, Nothing] => Stage[_I, _O, _E]): Panic = this

    private[stages] def complete[_I, _O, _E](f: Stage[Any, Nothing, Nothing] => Stage[_I, _O, _E]): Panic = this
  }

  def Panic(exception: Exception): Panic = Panic(NonEmptyChain.one(exception))
}
