package h8io.xi.stages

trait Decorator[-I, +O, +E] extends Stage[I, O, E] {
  val stage: Stage[I, O, E]

  override def dispose(): Unit = stage.dispose()
}
