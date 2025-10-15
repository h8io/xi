package h8io.xi.stages

@FunctionalInterface
trait Morphism[-IS <: Stage[?, ?, ?], +OS <: Stage[?, ?, ?]] extends (IS => OS) {
  def apply(stage: IS): OS

  @inline final def ∘[_IS <: Stage[?, ?, ?]](that: Morphism[_IS, IS]): Morphism[_IS, OS] = Morphism.Compose(this, that)

  @inline final def ~>[_IS <: Stage[?, ?, ?]](that: Morphism[_IS, IS]): Morphism[_IS, OS] = this ∘ that

  @inline final def <~[_OS <: Stage[?, ?, ?]](that: Morphism[OS, _OS]): Morphism[IS, _OS] = that ∘ this

  @inline final def ⋅(stage: IS): OS = this(stage)

  @inline final def <|(stage: IS): OS = this ⋅ stage
}

object Morphism {
  type Endo[I, O, E] = Morphism[Stage[I, O, E], Stage[I, O, E]]

  final case class Compose[IS <: Stage[?, ?, ?], MS <: Stage[?, ?, ?], OS <: Stage[?, ?, ?]](
      outer: Morphism[MS, OS], inner: Morphism[IS, MS])
      extends Morphism[IS, OS] {
    def apply(stage: IS): OS = outer(inner(stage))
  }
}
