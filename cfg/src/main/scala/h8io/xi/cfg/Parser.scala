package h8io.xi.cfg

import cats.data.ValidatedNec
import com.typesafe.config.ConfigValue
import h8io.xi.cfg.errors.CfgError

trait Parser[T] extends (Option[ConfigValue] => ValidatedNec[CfgError, T]) {
  final def apply(value: Option[ConfigValue]): ValidatedNec[CfgError, T] =
    value match {
      case Some(node) => this(node)
      case None => empty
    }
  def apply(node: ConfigValue): ValidatedNec[CfgError, T]
  val empty: ValidatedNec[CfgError, T]
}
