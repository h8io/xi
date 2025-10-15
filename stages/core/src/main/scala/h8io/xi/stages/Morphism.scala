package h8io.xi.stages

trait Morphism[-IS <: Stage[?, ?, ?], +OS <: Stage[?, ?, ?]] extends (IS => OS) {
  @inline final def ~>[_IS <: Stage[?, ?, ?]](that: Morphism[_IS, IS]): Morphism[_IS, OS] = stage => this(that(stage))

  @inline final def <~[_OS <: Stage[?, ?, ?]](that: Morphism[OS, _OS]): Morphism[IS, _OS] = that ~> this

  @inline final def <|(stage: IS): OS = apply(stage)
}
