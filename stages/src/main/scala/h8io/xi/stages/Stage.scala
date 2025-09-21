package h8io.xi.stages

trait Stage[-I, +O, +E] extends (I => Yield[I, O, E]) {
  def apply(in: I): Yield[I, O, E]

  final def ~>[_O, _E >: E](that: Stage[O, _O, _E]): Stage.Safe[I, _O, _E] = this.safe(_) ~> that

  // The type parameters of Yield.None are kept for backward compatibility with Scala 2.12
  private[stages] def safe(in: I): Yield[I, O, E] =
    try this(in)
    catch { case e: Exception => Yield.None[I, O, E](State.Panic(e).onDone) }
}

object Stage {
  trait Safe[-I, +O, +E] extends Stage[I, O, E] {
    final override private[stages] def safe(in: I): Yield[I, O, E] = this(in)
  }
}
