package h8io.xi.cfg.errors

import com.typesafe.config.ConfigOrigin

trait CfgError {
  val origin: ConfigOrigin
}
