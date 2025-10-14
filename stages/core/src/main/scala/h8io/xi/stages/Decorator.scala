package h8io.xi.stages

trait Decorator[II, IO, IE, -OI, +OO, +OE] extends (Stage[II, IO, IE] => Decorated[II, IO, IE, OI, OO, OE]) {}

object Decorator {
  type Endo[I, O, E] = Decorator[I, O, E, I, O, E]
}
