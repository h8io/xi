package h8io.xi.stages

import cats.data.Validated
import h8io.cfg.NodeError
import h8io.cfg.raw.Node
import h8io.reflect.Type

trait StageBuilder[E] {
  def apply[I](cfg: Node, in: Type[I]): Validated[NodeError, StageSpec[I, ?, E]]
}

object StageBuilder {}
