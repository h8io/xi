package h8io.xi.stages

import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite

trait StagesCoreTestUtil extends MockFactory {
  self: TestSuite =>

  def onDoneMock[I, O, E](onDone: OnDone[I, O, E], signal: Signal[E], stage: Stage[I, O, E]): Unit =
    signal match {
      case Signal.Success => (onDone.onSuccess _).expects().returns(stage)
      case Signal.Complete => (onDone.onComplete _).expects().returns(stage)
      case _: Signal.Error[E] => (onDone.onError _).expects().returns(stage)
    }
}
