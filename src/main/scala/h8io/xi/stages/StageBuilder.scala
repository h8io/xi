package h8io.xi.stages

import cats.data.Validated
import h8io.cfg.DecoderError
import h8io.cfg.raw.Node
import h8io.reflect.Type

trait StageBuilder[E] {
  def apply[I](cfg: Node, in: Type[I]): Validated[DecoderError, StageSpec[I, ?, E]]
}

object StageBuilder {}
