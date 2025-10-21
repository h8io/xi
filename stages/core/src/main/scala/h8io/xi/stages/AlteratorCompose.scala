package h8io.xi.stages

private[stages] final case class AlteratorCompose[-IS <: Stage.Any, MS <: Stage.Any, +OS <: Stage.Any](
    outer: Alterator[MS, OS], inner: Alterator[IS, MS])
    extends Alterator[IS, OS] {
  @inline def apply(stage: IS): OS = outer(inner(stage))
}
