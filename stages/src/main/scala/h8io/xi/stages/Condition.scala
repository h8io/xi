package h8io.xi.stages

trait Condition {
  self =>

  def check: Boolean
  def advance: Condition
  def reset: Condition
}

object Condition {
  object True extends Condition with (() => Condition) {
    def check: Boolean = true
    def advance: Condition = this
    def reset: Condition = this

    def apply(): Condition = this
  }

  object False extends Condition with (() => Condition) {
    def check: Boolean = false
    def advance: Condition = this
    def reset: Condition = this

    def apply(): Condition = this
  }
}
