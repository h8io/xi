package h8io.xi.stages

final case class AlterationCompose[IS <: Stage[?, ?, ?], MS <: Stage[?, ?, ?], OS <: Stage[?, ?, ?]](
    outer: Alteration[MS, OS], inner: Alteration[IS, MS])
    extends Alteration[IS, OS] {
  def apply(stage: IS): OS = outer(inner(stage))
}
