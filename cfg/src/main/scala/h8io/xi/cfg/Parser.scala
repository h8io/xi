package h8io.xi.cfg

import cats.data.ValidatedNec
import com.typesafe.config.ConfigValue
import h8io.xi.cfg.errors.CfgError

trait Parser[T] {
  def apply(node: ConfigValue): ValidatedNec[CfgError, T]
  val empty: Option[T] = None
}
