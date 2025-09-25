package h8io.xi.stages.util.conditions

import h8io.xi.stages.Condition

object LocalCountdown {
  private[conditions] final case class Impl(i: Long, n: Long) extends Condition {
    assume(n > 0, s"n must be > 0, got n=$n")
    assume(0 <= i && i <= n, s"i must be in [0, $n], got i=$i")

    def check: Boolean = i > 0
    def advance: Condition = Impl(i - 1, n)
    def reset: Condition = Impl(n, n)
  }

  def apply(n: Long): Condition = if (n > 0) Impl(n, n) else Condition.False
}
