package h8io.xi

package object stages {
  type Alteration[-IS <: Stage.Any, +OS <: Stage.Any] = IS => OS

  type Decorator[I, O, E] = Alteration[Stage[I, O, E], Stage[I, O, E]]

  implicit final class AlterationOps[-IS <: Stage.Any, +OS <: Stage.Any](val alteration: Alteration[IS, OS])
      extends AnyVal {
    @inline def ∘[_IS <: Stage.Any](that: Alteration[_IS, IS]): Alteration[_IS, OS] =
      AlterationCompose(alteration, that)

    @inline def ~>[_IS <: Stage.Any](that: Alteration[_IS, IS]): Alteration[_IS, OS] = alteration ∘ that

    @inline def <|[_IS <: Stage.Any](that: Alteration[_IS, IS]): Alteration[_IS, OS] = alteration ∘ that

    @inline def <~[_OS <: Stage.Any](that: Alteration[OS, _OS]): Alteration[IS, _OS] = that ∘ alteration

    @inline def ⋅(stage: IS): OS = alteration(stage)

    @inline def <|(stage: IS): OS = this ⋅ stage
  }
}
