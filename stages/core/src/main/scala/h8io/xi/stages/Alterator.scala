package h8io.xi.stages

trait Alterator[+S <: Stage.Any, -I, +O, +E] extends Stage[I, O, E] {
  val stage: S

  override def dispose(): Unit = stage.dispose()
}
