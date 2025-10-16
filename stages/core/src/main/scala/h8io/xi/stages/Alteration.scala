package h8io.xi.stages

@FunctionalInterface
trait Alteration[-IS <: Stage[?, ?, ?], +OS <: Stage[?, ?, ?]] extends (IS => OS) {
  def apply(stage: IS): OS

  @inline final def ∘[_IS <: Stage[?, ?, ?]](that: Alteration[_IS, IS]): Alteration[_IS, OS] =
    Alteration.Compose(this, that)

  @inline final def ~>[_IS <: Stage[?, ?, ?]](that: Alteration[_IS, IS]): Alteration[_IS, OS] = this ∘ that

  @inline final def <~[_OS <: Stage[?, ?, ?]](that: Alteration[OS, _OS]): Alteration[IS, _OS] = that ∘ this

  @inline final def ⋅(stage: IS): OS = this(stage)

  @inline final def <|(stage: IS): OS = this ⋅ stage
}

object Alteration {
  type Endo[I, O, E] = Alteration[Stage[I, O, E], Stage[I, O, E]]

  final case class Compose[IS <: Stage[?, ?, ?], MS <: Stage[?, ?, ?], OS <: Stage[?, ?, ?]](
      outer: Alteration[MS, OS], inner: Alteration[IS, MS])
      extends Alteration[IS, OS] {
    def apply(stage: IS): OS = outer(inner(stage))
  }
}
