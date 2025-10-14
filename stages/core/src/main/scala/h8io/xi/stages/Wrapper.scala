package h8io.xi.stages

trait Wrapper[-II, +IO, +IE, -OI, +OO, +OE] extends Stage[OI, OO, OE] {
  val stage: Stage[II, IO, IE]

  override def dispose(): Unit = stage.dispose()
}

object Wrapper {
  type Endo[-I, +O, +E] = Wrapper[I, O, E, I, O, E]
}
