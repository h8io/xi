package h8io.xi.cfg

import cats.data.ValidatedNec
import com.typesafe.config.ConfigValue
import h8io.xi.cfg.errors.CfgError

@FunctionalInterface
trait Parser[T] extends (ConfigValue => ValidatedNec[CfgError, T]) {
  def apply(node: ConfigValue): ValidatedNec[CfgError, T]
  val empty: Option[T] = None

  final def map[U](f: T => U): Parser[U] = this(_) map f
}
