package h8io.xi

package object stages {
  type Alterator[-IS <: Stage.Any, +OS <: Stage.Any] = IS => OS

  type Decorator[I, O, E] = Alterator[Stage[I, O, E], Stage[I, O, E]]

  implicit final class AlteratorOps[-IS <: Stage.Any, +OS <: Stage.Any](val alterator: Alterator[IS, OS])
      extends AnyVal {
    @inline def ∘[_IS <: Stage.Any](that: Alterator[_IS, IS]): Alterator[_IS, OS] = AlteratorCompose(alterator, that)

    @inline def ~>[_IS <: Stage.Any](that: Alterator[_IS, IS]): Alterator[_IS, OS] = alterator ∘ that

    @inline def <|[_IS <: Stage.Any](that: Alterator[_IS, IS]): Alterator[_IS, OS] = alterator ∘ that

    @inline def <~[_OS <: Stage.Any](that: Alterator[OS, _OS]): Alterator[IS, _OS] = that ∘ alterator

    @inline def ⋅(stage: IS): OS = alterator(stage)

    @inline def <|(stage: IS): OS = this ⋅ stage
  }
}
