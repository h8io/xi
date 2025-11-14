package h8io.xi.stages

import h8io.reflect.Type
import h8io.stages.Stage

final case class StageSpec[-I, O, +E](out: Type[O], stage: Stage[I, O, E])
