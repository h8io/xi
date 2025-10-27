package h8io.xi.stages

trait Alterator[+S <: Stage.Any, -I, +O, +E] extends Stage[I, O, E] {
  val underlying: S

  override def dispose(): Unit = underlying.dispose()
}
