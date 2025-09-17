package h8io.xi

import scala.annotation.tailrec

object Factorial {
  def apply(n: Int): BigInt = apply(n, BigInt(1))

  @tailrec private def apply(n: Int, acc: BigInt): BigInt = if (n < 2) acc else apply(n - 1, acc * n)
}
