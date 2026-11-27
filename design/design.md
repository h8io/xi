# xi — design notes

Working document for xi: how cfg configuration becomes stages pipelines. **Status:** almost nothing of
this is implemented. `StageBuilder` and `StageSpec` (§7) exist as drafts; the rest of the first draft
(`Tagged`, built on cfg-schema) was removed when cfg 0.4.x dropped cfg-schema, and the design is being
written again against the new cfg.

§2 lists what is settled, §9 what is still open, and §10 logs how each decision was reached, so that a later
session does not reopen a question that was already argued through. Add to §10 rather than silently editing
§2. None of the signatures below are settled; read them for intent, not as a contract.

## 1. What xi is for

xi turns [cfg](https://cfg.h8.io/) configuration into [stages](https://stages.h8.io/) pipelines: it reads a
cfg node tree and builds stages wired together, so that a pipeline can be described in a config file rather
than in code. The logic of that conversion is domain-agnostic; the stages it builds are not, and come from
domain modules, Apache Spark being the first.

## 2. Decisions already taken

| Decision | Where |
| --- | --- |
| xi builds one `Stage` from a config and an input type given by its caller, and returns it with its output type; nothing else | §4 |
| xi takes the config already loaded and merged; the pipeline is its root field `xi`, a sequence of tagged nodes (being replaced: `xi` becomes a map, §9) | §4 |
| A later source replaces `xi` whole; what is to be overridden lives outside it and is reached with `${…}` | §4 |
| A pipeline is a `Stage`: a list of tagged nodes folded with `~>`, nesting wherever a stage takes a stage; an empty list is `Identity`, one tagged node alone a pipeline of one stage | §4 |
| Configs are Psi or YAML; a tag is a node tag, written in the source, and nothing else | §5 |
| Two modules: core (domain-agnostic conversion) and spark (Spark-bound stages) | §3 |
| spark is built for Scala 2.13 only; core is cross-built over 2.13 and 3.9 | §3 |
| Spark stages work on `DataFrame`, not on a typed `Dataset[T]` | §3 |
| The tag selects the builder; a tag is the FQN of a stage class or an alias for one (being reworked: tags from `xi.stages`, §9) | §5 |
| Each domain module brings its own alias table; core only consults the tables it is given (being reworked with it, §9) | §5 |
| Builders only, for now: a builder takes the input type and the stage's cfg `Node`, checks that its stage can be built on that input, and returns the stage with its output type; a tag never names a constructor | §6, §7 |
| A builder is any object that is a `StageBuilder`; by convention the stage's companion, in the same module, which parses its own parameters | §6 |
| core finds the builder by reflection from the FQN, and uses reflection for nothing else | §6 |
| Types are carried at runtime; a builder returns the stage together with the witness of its output type | §7 |
| Errors are accumulated, not thrown; xi's own `XiError`, carrying as many `Location`s as the error has, none included | §8 |

## 3. Modules

- **core** (`xi-core`) — domain-agnostic. Everything that turns configuration into a pipeline lives here:
  resolving a node's tag, finding the builder for it, folding chains, descending into nested pipelines,
  carrying types at runtime and accumulating errors. It knows nothing about Spark or any other domain.
  Cross-built over 2.13 and 3.9.
- **spark** (`xi-spark`) — simple stages bound to Apache Spark, each with its builder next to it, plus the
  table of aliases for this domain. Depends on core; Spark itself is `Provided`.

**spark is built for Scala 2.13 only**, because Spark publishes no Scala 3 build. It is not going to be
reached from Scala 3 through `CrossVersion.for3Use2_13` either: `DataFrame` operations would work that way,
but `spark.implicits._` and `Encoder` derivation rest on scala-reflect's `TypeTag`, which Scala 3 does not
have. So on 3.9 the module is empty — no sources, no Spark dependency, nothing published — and the Scala 3
API docs have no spark in them. It stays in the 3.9 row of the cross build rather than leaving it, because
sbt's `+` runs a task on the aggregating root alone wherever the root supports the version, and the root
would then pull a 2.13-only spark onto a core switched to 3.9 (§10, session 1).

**Spark stages work on `DataFrame`** (`Dataset[Row]`), not on a typed `Dataset[T]`. A typed dataset needs an
`Encoder[T]`, and when the pipeline comes from config `T` is only known at runtime, where no implicit can be
summoned for it. The consequence is that a spark stage's input and output are almost always `DataFrame`: the
type computations of §7 are mostly needed by the generic stages of `h8io.stages` lib, not by the spark ones.

## 4. Pipelines

**What xi does, and all it does:** given a config and an input type, it builds the pipeline — one
`Stage` — and gives its output type. The input type is the caller's: xi accepts any, and does not
assume one (a pipeline that starts from nothing is simply one whose caller passes `Unit`). Running the
pipeline, and loading the config, are not xi's.

**The config arrives loaded and merged.** An application may be configured from a list of sources;
merging them is the loader's work (Psi's, which also brings YAML in through `#yaml(…)`), and xi takes the
one root map it produces. The pipeline is that map's **root field `xi`**, so a pipeline can live in the
same files as the rest of an application's settings:

```
xi: [
  @read { path: ${input.path} }
  @filter { condition: ${filter.condition} }
]
```

**A later source replaces the pipeline whole.** cfg merges maps deeply, but a sequence is replaced
wholesale and there is no append operator (cfg `design/format.md` §7), and no path addresses the element
of a sequence. So a source after the one that defines `xi` can swap the pipeline for another, but cannot
insert a stage into it or change a parameter of one. That is accepted: a value meant to be overridden is
written outside `xi`, in an ordinary map, and the stage reaches it with `${…}`, as above. Rejected: `xi`
as a map of named steps, which would make every parameter overridable but would put the order of the
pipeline on the order of the keys.

That convention holds for Psi only. cfg's YAML loader also merges a list of files, last wins, sequences
replaced, but it has no substitution step — a YAML file is taken as written, and its anchors and `<<`
reach only within one document. So a pipeline loaded from YAML files alone has no way to override a
stage's parameter short of replacing `xi`; loaded through Psi, with YAML brought in by `#yaml(…)`, it
has `${…}` again. Psi is the loader xi is designed around, and a configuration of YAML files alone is not
a case the design caters for: it works, without overrides of parameters.

**A pipeline is a `Stage`.** `Stage ~> Stage` is itself a `Stage`, so there is no separate "pipeline"
concept to model — in config as in code, a pipeline is just a stage that happens to be built out of
others: a **chain**, a list of tagged nodes, folded left with `~>` in list order. An empty list, `xi: []`,
is `std.Identity` of the input type: its output type is its input type. A single tagged node, not in a
list, is a pipeline as well — the same as a list of that one node — at every level, since the decoder is
one: `xi`'s pipeline and a pipeline parameter of a nested stage alike.

And it nests: a stage's own configuration can take **a pipeline as a parameter**. `cycles.Loop` and
`cycles.Repeat` each wrap one inner stage; `operators.And` and `operators.Or` each take two branches. The
parameter positions in those configs accept exactly what `xi` does, so the pipeline decoder is recursive
and reused wherever a stage asks for a stage. A nested pipeline's input is the input of the stage that
holds it, not the caller's: the decoder takes the input type as a parameter at every level, and only the
top level gets it from the caller.

## 5. Tags

Every stage type comes with a **`StageBuilder`**, the object that turns one config node into the `Stage`
itself. **The tag selects the builder.** A stage is written `@stage-tag stage-cfg` in Psi and
`!stage-tag stage-cfg` in YAML, where `stage-cfg` is any cfg node — null, scalar, map or sequence — and is
handed to the builder as it is. **The node tag is the only spelling:** a node with no tag where a stage is
expected is an error on that node. A stage with no parameters is `@identity null` in Psi, which needs a
value after a tag, and `!identity` in YAML — where it is an empty *scalar*, not a null: an explicit tag
takes the node out of YAML's null resolution (cfg-yaml wraps a scalar as `Node.Null` only when its tag is
`!!null`), and so does `!identity ~`. A builder that takes no parameters has to accept both.

**Configs are Psi or YAML**, because those are the loaders that produce node tags. The HOCON/JSON loader
goes through Typesafe Config, which has no notion of a tag — every node it builds has `tag == None` — so
a pipeline cannot be written in HOCON. HOCON can still reach a config through Psi's `#hocon(…)`, untagged,
which makes it a place for the parameters a stage refers to with `${…}` (§4), not for stages. Dropped with
this (§10, session 2): the three spellings the first draft carved out of map structure for HOCON — a `_`
key holding the tag, a single-key map, a bare scalar — and their errors, `AmbiguousMap` and
`NonScalarTag`.

A tag is either **the FQN of a stage class** or **an alias** for one. Aliases depend on the domain the
pipeline is written for, so each domain module brings its own table (spark has one for its stages); core
only knows how to consult the tables it is given and expand an alias into an FQN.

## 6. Builders and their lookup

**Builders only, for now.** What builds a stage from config is a builder: it takes the input type and the
cfg of that one stage, checks that its stage can be built on that input, and if so builds it, returning the
stage with its output type. Other ways — a factory method, say — may be added later; none is designed now.

**A builder is any object that is a `StageBuilder`.** By convention it is the stage's companion, in the
same module as the stage: `object Filter extends StageBuilder[...]` next to `class Filter`, so the FQN of
the stage and of its builder are one string. But nothing requires it: a builder in core for stages without
parameters, named in `stages` by its own FQN, serves as well. It parses its own parameters from the config
node. **core finds it by reflection**, from the FQN: it loads the class of the object (`<FQN>$`) and takes
its `MODULE$`, which both 2.13 and 3.9 emit for an object. Reflection is used only to *find* the builder —
core never calls a stage's constructor or reads its parameters itself. Three things this lookup has to get
right:

1. **Check before initializing.** The tag comes from config, which is effectively outside input, and
   `Class.forName(name)` with initialization runs the static initializer of whatever class it names. Load
   with `initialize = false`, check `classOf[StageBuilder[?]].isAssignableFrom(cls)`, and only then read
   `MODULE$`. A class that fails the check is an `XiError` on the tag's node, not an exception.
2. **The right classloader.** Under Spark, user jars (`--jars`, `spark.jars`) are loaded by a classloader of
   their own, which the one that loaded xi-core cannot see into. Use the thread's context classloader, or
   one passed in explicitly — not `getClass.getClassLoader`.
3. **A precise error.** A stage class without a companion object has no `<FQN>$`, and the error has to say
   that the class has no builder, not that the class was not found.

**The builder reads `Node`, for now.** A builder gets the stage's cfg as a cfg `Node` and reads it itself.
Decoding into case classes, à la circe, comes later, as a layer over this one — a builder for a `T` made
from a decoder of `Node` into `T` — so that it adds to the `Node` builder rather than replacing it.

## 7. Runtime types

Types are carried at runtime, because the pipeline shape is only known once the config is read. The two
drafts that exist in code — an illustration of the idea, not a signature to follow to the letter (it
returns `NodeError`, where §8 now has `XiError`):

```scala
trait StageBuilder[E] {
  def apply[I](cfg: Node, in: Type[I]): Validated[NodeError, StageSpec[I, ?, E]]
}
final case class StageSpec[-I, O, +E](out: Type[O], stage: Stage[I, O, E])
```

**`StageSpec` is the builder's result**: the stage it built, plus the witness of that stage's output type.
It exists because of erasure. A stage assembled from config is abstract — the return type is
`StageSpec[I, ?, E]`, and `?` is exactly the point: what `O` is was decided at runtime, from the config,
so nothing static remains of it. Pairing `out: Type[O]` with `stage: Stage[I, O, E]` in one value is what
keeps the erased `O` recoverable and keeps the witness from drifting away from the stage it describes.

That is what makes a chain fold: each element's `StageSpec.out` becomes the next element's `in`, and the
stages compose with `Stage.~>`. At that seam the compiler cannot help — the two `?`s are unrelated to it —
so the `Type` comparison is what stands in for the typecheck, and whatever cast joins the two stages is
sound only because that comparison passed. Keep the check and the cast next to each other.

The same `StageSpec` comes back from a nested pipeline: a builder for `And` decodes its two branches, gets
a `StageSpec` for each, and derives its own output type from theirs.

### Why runtime types at all

Because **many stages derive their output type from their input type**, and the input type is only known
once the config has been read. The stages library is full of polymorphic singletons cast to a concrete
type at the use site, where the type parameter comes from the caller and nothing else:

| Stage (`h8io.stages`) | Input | Output |
| --- | --- | --- |
| `std.Identity[T]` | `T` | `T` |
| `projections.Unlift[T]` | `Option[T]` | `T` |
| `std.Coalesce[T]` | `Either[T, T]` | `T` |
| `std.Swap[L, R]` | `(L, R)` | `(R, L)` |
| `projections.Either.Left[T]` | `Either[T, ?]` | `T` |
| `operators.And(l, r)` | `I` | `(LO, RO)` — from the branches' outputs |
| `operators.Or(l, r)` | `I` | `Either[LO, RO]` — likewise |
| `cycles.Loop(s)` | `T` | `T` (the inner stage is an endo) |
| `cycles.Repeat(s)` | `I` | the inner stage's output |

A builder for any of these cannot summon `Tag[O]` implicitly — `O` is a function of the runtime `in`, or
of what a nested pipeline turned out to produce. It has to *compute* the output tag: pass it through
(`Identity`, `Repeat`), take the input tag apart (`Unlift`, `Coalesce`), take it apart and rebuild it in a
new shape (`Swap`), or build a new applied type out of the tags two nested pipelines reported (`And`,
`Or`). And it has to check the input in the first place — `Unlift` is only applicable if
`in <:< Option[?]`, `Loop` only if its inner stage's output matches its input — otherwise the builder
reports an `XiError` against that node's `Location`.

That is the argument for izumi-reflect over `h8io.reflect`: `LightTypeTag` has `typeArgs`, `withoutArgs`
and `combine` for exactly this taking-apart and rebuilding, alongside `<:<`/`=:=`, while
`h8io.reflect.Type` exposes only the comparisons. But izumi's comparisons are wrong exactly at the
`Any`/`AnyVal`/`AnyRef`/`Null` boundary, which is why `h8io.reflect` was written, and `h8io.reflect` fixes
them only at the top of a type, not inside its arguments (measured in session 3, §9). Neither library is
enough as it is; treat the `h8io.reflect.Type` in `StageBuilder`/`StageSpec` as provisional (§9).

## 8. Errors

Errors are accumulated rather than thrown, so that a bad config reports every problem with its source
`Location` at once. **The error is xi's own `XiError`, not cfg's `NodeError`**, because an error of xi is
not always about one node: a mismatch of types at the seam of two stages is about the output of one and the
input of the next, and a missing field such as `pipeline` is about a node that is not there, so only its
parent can be pointed at. An `XiError` carries as many `Location`s as its error has, none included. The
draft `StageBuilder` returns `Validated[NodeError, _]`, which used to be cfg-schema's `CfgValue`; what
accumulates the errors is decided during implementation (§9).

## 9. Open questions

### The format of `xi` (session 3, in progress)

xi starts from the config: what it is for is that pipelines are convenient to write, so the format is
settled first and the code follows it. Agreed so far:

- `xi` is a **map** with at least two fields: **`stages`**, a mapping of stage tags to what builds the
  stage (a builder — the only kind for now, §6), and **`pipeline`**, the sequence of `@stage-tag stage-cfg`
  that `xi` itself used to be (§4). `stages` is a map, so it merges across sources and each can add tags;
  `pipeline` is a sequence and is still replaced whole.
- **An FQN in `stages` names a builder**, an object that is a `StageBuilder` (§6) — the stage's
  companion by convention, any such object in fact — checked by reflection, not declared by the author.
- **An entry in `stages` is an FQN, a scalar, and nothing more.** No tags derived from others: a stage
  with its cfg preset is written with Psi's injection (cfg `design/format.md` §9), not by xi. `$a*` among
  the fields of a block puts `a`'s fields there; what follows overrides them as `:` merges, maps deep and
  anything else replaced, and a `:=` after it replaces whole. The injected fields keep their own
  locations, and Psi reports a cycle of substitutions itself. A graft, `- $presets.strict` with
  `strict: @filter { mode: strict }`, brings a whole stage, its tag included, but cannot be added to.

```
presets {
  strict { mode: strict, options { a: 1, b: 2 } }
}
xi {
  stages {
    filter: h8io.xi.spark.Filter                          # an FQN, the builder found by reflection
  }
  pipeline: [
    @filter { $presets.strict*, condition: "x > 0" }      # the preset, plus a field of its own
    @filter { $presets.strict*, options := { c: 3 } }     # the preset, with options replaced whole
  ]
}
```

Open:

- **Tags in `pipeline` only from `stages`**, no FQN as a tag. Proposed: one lookup, an unknown tag
  reported as missing from `stages` rather than as a class not found, and no backticks in Psi tags.
- **A later source redefining a tag in `stages`**: a feature, swapping an implementation, or something to
  warn about, since it changes what a pipeline runs without a trace.
- **Unknown fields in `xi`**, such as a misspelt `pipline`: an error — proposed — or ignored.
- **Where presets live.** Outside `xi`, as `presets` above, which xi never reads; or a field of `xi`
  such as `xi.presets`, which only Psi reads, and which unknown fields being an error would have to allow.
- **Where the domain modules' tables go.** §5 has each domain module bring an alias table in code. With
  `stages` in the config, spark could ship a Psi resource with its `xi.stages { … }` instead, included with
  `#include(…)`: one mechanism for aliases rather than two, and all of it in config.

They are not independent, and the first is the one to take first: with tags in `pipeline` only from
`stages`, the backticks of FQN tags in Psi (below) never arise, and the alias table (below) becomes the
question of where the domain modules' `stages` entries go, the last one above.

### Other questions

- **What accumulates errors.** cfg-schema gave `CfgValue`, an alias of cats' `Validated`; cfg 0.4.x has
  neither. cats-core is still on the classpath, but only transitively, through the `io.h8 %% stages`
  umbrella artifact and its `stages-cats`. Options: depend on cats-core directly, or a small accumulating
  type of xi's own. Not to be settled in advance: decided during implementation, by what the builders
  turn out to need.
- **izumi-reflect or `h8io.reflect`.** `h8io.reflect` exists because izumi does not get every type right.
  Measured in session 3 on izumi-reflect 3.0.11, Scala 2.13.18 and 3.9.0 alike, `LightTypeTag.<:<` answers
  `true` for all of: `Int <: AnyRef`, `AnyVal <: AnyRef`, `Any <: AnyRef`, a universal trait or a value
  class `<: AnyRef`, and `Null <:` `Int`, `AnyVal`, a value class or `Nothing`. izumi treats `AnyRef` as
  `Any` and `Null` as a bottom type. `Int` and `java.lang.Integer`, and `Unit` and `BoxedUnit`, it does tell
  apart. `reflect` 0.0.6 corrects all of these — but only at the top of a type: inside type arguments it
  delegates to izumi, so `Box[Int] <: Box[AnyRef]`, `Box[Null] <: Box[Int]` (`Box` covariant) and
  `Stage[AnyRef, Int] <: Stage[Int, Int]` still come out `true` through `h8io.reflect.Type`. And §7 needs
  izumi's taking-apart (`typeArgs`, `combine`), which `reflect` does not expose. So the question is no
  longer which of the two, but how a type that compares correctly at every depth and can be taken apart is
  had: `reflect` growing both, or something else.
- **The `StageBuilder` signature**: the error type parameter `E`, how a builder receives what it needs to
  build nested pipelines (core's decoder, the alias tables), and whether the alias tables are passed in or
  discovered.
- **The alias table**: its format and how a domain module hands it to core.
- **FQN tags in Psi** need backticks — ``@`h8io.xi.spark.Filter` `` — because a dot does not fit
  `Id.SafeKeyPattern`. Aliases make that rare; whether anything more is needed is open.

## 10. Decision log

Each entry records what was chosen, and — where it matters — what was rejected and why. A rejected
alternative listed here should not be re-proposed without new information.

### Before session 1 — the first draft

Carried over from the first draft (`Tagged`, `StageBuilder`, `StageSpec`, written against cfg-schema and
of which `Tagged` was removed with it): a pipeline is a `Stage`, in two spellings, nesting (§4); the four tag spellings, three of
them a crutch for HOCON (§5); types carried at runtime with `StageSpec` pairing a stage and its output
witness (§7); accumulated errors (§8); izumi-reflect argued for over `h8io.reflect` (§7, §9).

### Session 1 — 2026-11-23

- **Two modules, core and spark.** core is domain-agnostic and holds all of the conversion; spark holds
  simple stages bound to Spark. core does not depend on Spark.
- **spark is 2.13 only.** Spark has no Scala 3 build. Rejected: using Spark's 2.13 artifacts from Scala 3
  through `CrossVersion.for3Use2_13` — `spark.implicits._` and `Encoder` derivation need scala-reflect's
  `TypeTag`, which Scala 3 lacks.
- **Spark stages on `DataFrame`.** Rejected: typed `Dataset[T]`, whose `Encoder[T]` cannot be summoned for
  a `T` known only at runtime.
- **Builder in the companion object, found by reflection.** The builder lives next to its stage and parses
  its own parameters; core uses reflection only to find it from the FQN (§6), with three conditions:
  check `StageBuilder` before initializing the class, use the context classloader, and report a missing
  companion as "no builder". Rejected: core instantiating stage classes by reflection from their
  constructors and the config's fields — parameter parsing would move into core and every mistake would
  surface only at runtime.
- **Aliases per domain module.** Each domain module brings its own table; core consults what it is given.
- **spark is empty on 3.9, not absent from it.** Tried first: `spark / crossScalaVersions := Seq("2.13.18")`.
  `+testFull` then failed with `Scala version mismatch: spark (Scala 2.13.18) depends on core (Scala 3.9.0)`:
  sbt 2.1.0's `Cross.scala` runs a task on the current project alone when it supports the version, and the
  root's aggregation reaches spark, which was left on 2.13. Rejected as well: dropping 3.9 from the root,
  which would leave coverage on 3.9 without the root that aggregates and gates it. So spark cross-builds
  over both and is empty on 3.9 (`sparkSupported` in `build.sbt`). This also settles the docs: `pages`
  depends on core and spark, and the Scala 3 API simply has no spark.

### Session 2 — 2026-11-23

- **What xi is for, narrowed.** xi builds one `Stage` from a config and an input type given by its caller,
  and returns it with its output type. Proposed first: every pipeline starts from `Unit`. Settled instead:
  any input type, the caller's choice — `Unit` is just what a caller passes for a pipeline that starts from
  nothing. Nested pipelines take the input of the stage that holds them either way, so the decoder carries
  the input type at every level.
- **The pipeline is the root field `xi`** of a config that arrives loaded and merged: a sequence of
  `@stage-tag stage-cfg`, `stage-cfg` any cfg node. Loading a list of sources and merging them is the
  loader's work, not xi's.
- **A later source replaces `xi` whole.** cfg replaces sequences on merge and has no append operator, so
  a later source cannot insert a stage or change one's parameter. Accepted, with the convention that what
  is to be overridden lives outside `xi` and is reached with `${…}` — which works under Psi only, cfg's
  YAML loader having no substitution. Rejected: `xi` as a map of named steps, which would make every
  parameter overridable at the price of ordering the pipeline by its keys.
  That limit was judged not to matter: YAML on its own is not a case worth designing for, YAML comes in
  through Psi's `#yaml(…)`.
- **Psi and YAML only; the node tag is the only spelling.** Dropped: HOCON as a format for pipelines, and
  with it the three spellings the first draft carved out of map structure for it (`_`, the single-key map,
  the bare scalar) and the errors `AmbiguousMap` and `NonScalarTag`. HOCON stays reachable through Psi's
  `#hocon(…)` for untagged parameters.
- **An empty pipeline is `Identity`** of the input type, at every level. Noted with it: inside a cycle
  it never returns (§9).
- **Constructor or builder, reopened** (§9). The proposal was a tag bound to "a constructor or a builder";
  session 1 had rejected core calling constructors. Not decided either way yet.

### Session 3 — 2026-11-24

- **The format first.** xi starts from the config, its aim being that pipelines are convenient to write,
  so the format of `xi` is settled before the code.
- **`xi` becomes a map** with at least `stages`, tags mapped to what builds each stage, and `pipeline`,
  the sequence `xi` itself was in session 2.
- **The kind of an FQN in `stages` is found by reflection**, not declared by a tag on the value. Rejected:
  `filter: @builder h8io.xi.spark.Filter` — explicit and precise in its errors, but more to write, and
  convenience of writing is the aim.
- **An entry in `stages` can be more than an FQN.** What it is, and the rest of the format, are open
  (§9, the format of `xi`).
- **Builder, not constructor.** A tag names a builder; core never calls a stage's constructor. Session 1's
  decision stands, and with it the constructor questions of the format go: matching map fields to
  parameter names, decoding a parameter, reading the output type from the class's `Stage` supertype.
- **A single tagged node is a pipeline**, the same as a list of that one node, at every level.
- **No special rule for an empty pipeline inside a cycle.** `@loop []` never returns, and that is the
  author's to avoid: the cycles' builders are like every other builder and do not reject it.
- **Error accumulation is decided during implementation**, not in advance.
- **izumi gets the `Any`/`AnyVal`/`AnyRef`/`Null` boundary wrong, and `reflect` fixes it only at the top
  level.** Measured, not argued: the cases are listed in §9. The izumi-or-`reflect` question becomes how
  to have both correct comparisons at every depth and the taking-apart §7 needs.
- **The `StageBuilder` signature stays open** (§9), deliberately.
- **Presets are Psi's injections; a `stages` entry is an FQN only.** Proposed first: an entry could be
  `@other-tag cfg`, a tag derived from another with its cfg preset, merged with the use site's cfg. cfg's
  merge lives in Psi's loader, over syntax trees before resolution, with `:=` and a `MergedLocation`; a
  loaded `Node` keeps neither. So rejected: (a) xi merging `Node`s itself and (b) cfg making a merge of
  `Node`s public — both a second merge rule over a poorer representation, both without `:=`, so a use site
  could never replace a map of the preset, and (b) would add a generic `IMap` and a merged `Location` to
  cfg's API; and (c) no merge, `@strict-filter null` taking the preset as is — a preset that cannot be
  added to. Psi's `$a*`, in cfg 0.4.0 on, already does it with the one rule, `:=` and the right locations,
  and catches cycles; xi needs no code for it. Given up: presets work under Psi only (YAML's `<<` reaches
  within one document, and YAML alone is not a case designed for, §4), and the use site writes the base
  tag — `@filter { $presets.strict*, … }` — so a variant of a stage has no name of its own as a tag. This
  closes what a richer entry is, how a preset merges, and chains and cycles of derived tags; the entry
  above, that an entry can be more than an FQN, is withdrawn.
- **Builders only, for now.** A builder takes the input type and the stage's cfg, checks that its stage
  can be built on that input, and builds it, returning the stage with its output type. Other ways, the
  factory among them, may come later and are not designed now; the open question of what a factory is
  goes. `StageBuilder` and `StageSpec` illustrate the idea and are not to be followed to the letter.
- **A builder is any `StageBuilder` object**, the stage's companion by convention only. Loading `<FQN>$`
  finds a non-companion object as well, and allowing it costs nothing: a generic builder in core covers
  stages without parameters, the boilerplate that made a constructor look attractive.
- **The builder reads `Node` for now**; case classes decoded à la circe come later, as a layer over it.
  This answers what replaces cfg-schema's `Decoder`, for now.
- **`XiError` instead of cfg's `NodeError`.** An error of xi can be about several nodes — a mismatch of
  types between two stages — or about one that is absent, a missing field, so an `XiError` carries any
  number of `Location`s, none included.
