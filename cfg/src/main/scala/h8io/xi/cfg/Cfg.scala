package h8io.xi.cfg

import h8io.xi.cfg.errors.CfgError

trait Cfg {
  def apply[T](key: String)(parser: Parser[T]): ValidatedNec[CfgError, T]
}
