package h8io.xi.cfg.parsers

import cats.data.ValidatedNec
import cats.syntax.all.*
import com.typesafe.config.{ConfigList, ConfigValue}
import h8io.xi.cfg.Parser
import h8io.xi.cfg.errors.CfgError

import scala.jdk.CollectionConverters.*

trait Basic {
  implicit final def option[T](implicit parser: Parser[T]): Parser[Option[T]] =
    new Parser[Option[T]] {
      def apply(node: ConfigValue): ValidatedNec[CfgError, Option[T]] = parser(node) map (Some(_))

      override val empty: Some[Option[T]] = Some(None)
    }

  implicit final def list[T](implicit parser: Parser[T]): Parser[List[T]] =
    new Parser[List[T]] {
      def apply(node: ConfigValue): ValidatedNec[CfgError, List[T]] =
        node match {
          case list: ConfigList => list.iterator.asScala.map(parser(_)).toList.sequence
          case value => parser(value) map (_ :: Nil)
        }

      override val empty: Option[List[T]] = Some(Nil)
    }
}
