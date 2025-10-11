package h8io.xi.stages.util

import h8io.xi.stages.Stage

trait Decorator[-I, +O, +E] extends Stage[I, O, E] {
  val stage: Stage[I, O, E]

  override def dispose(): Unit = stage.dispose()
}
