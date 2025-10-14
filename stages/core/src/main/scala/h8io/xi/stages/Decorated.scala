package h8io.xi.stages

trait Decorated[-II, +IO, +IE, -OI, +OO, +OE] extends Stage[OI, OO, OE] {
  val stage: Stage[II, IO, IE]

  override def dispose(): Unit = stage.dispose()
}

object Decorated {
  type Endo[-I, +O, +E] = Decorated[I, O, E, I, O, E]
}
