package h8io.xi.cfg

import cats.data.ValidatedNec
import h8io.xi.cfg.errors.CfgError

trait Cfg {
  def apply[T](key: String)(parser: Parser[T]): ValidatedNec[CfgError, T]
}
