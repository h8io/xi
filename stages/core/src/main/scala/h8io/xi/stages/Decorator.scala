package h8io.xi.stages

trait Decorator[+II, -IO, -IE, -OI, +OO, +OE] extends (Stage[II, IO, IE] => Stage[OI, OO, OE]) {
  @inline final def ~>[_II, _IO, _IE](
      that: Decorator[_II, _IO, _IE, II, IO, IE]): Decorator[_II, _IO, _IE, OI, OO, OE] = stage => this(that(stage))

  @inline final def ~>(stage: Stage[II, IO, IE]): Stage[OI, OO, OE] = this(stage)

}

object Decorator {
  type Endo[I, O, E] = Decorator[I, O, E, I, O, E]
}
