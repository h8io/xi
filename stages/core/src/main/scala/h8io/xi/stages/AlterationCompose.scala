package h8io.xi.stages

private[stages] final case class AlterationCompose[-IS <: Stage[?, ?, ?], MS <: Stage[?, ?, ?], +OS <: Stage[?, ?, ?]](
    outer: Alteration[MS, OS], inner: Alteration[IS, MS])
    extends Alteration[IS, OS] {
  @inline def apply(stage: IS): OS = outer(inner(stage))
}
