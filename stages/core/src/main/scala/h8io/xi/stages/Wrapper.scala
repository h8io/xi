package h8io.xi.stages

trait Wrapper[+S <: Stage.Any, -I, +O, +E] extends Stage[I, O, E] {
  val stage: S

  override def dispose(): Unit = stage.dispose()
}

object Wrapper {
  type Endo[-I, +O, +E] = Wrapper[Stage[I, O, E], I, O, E]
}
