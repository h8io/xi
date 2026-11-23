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
| A pipeline is a `Stage`: a single tagged node or a list of them folded with `~>`, nesting wherever a stage takes a stage | §4 |
| Two modules: core (domain-agnostic conversion) and spark (Spark-bound stages) | §3 |
| spark is built for Scala 2.13 only; core is cross-built over 2.13 and 3.9 | §3 |
| Spark stages work on `DataFrame`, not on a typed `Dataset[T]` | §3 |
| The tag selects the builder; a tag is the FQN of a stage class or an alias for one | §5 |
| Each domain module brings its own alias table; core only consults the tables it is given | §5 |
| The builder is the stage's companion object, in the same module as the stage, and parses its own parameters | §6 |
| core finds the builder by reflection from the FQN, and uses reflection for nothing else | §6 |
| Types are carried at runtime; a builder returns the stage together with the witness of its output type | §7 |
| Errors are accumulated, not thrown, each with its node's `Location` | §8 |

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
have. So the module sets its own `crossScalaVersions` to 2.13 alone, `+` commands skip it on the 3.9 row,
and the Scala 3 API docs have no spark in them.

**Spark stages work on `DataFrame`** (`Dataset[Row]`), not on a typed `Dataset[T]`. A typed dataset needs an
`Encoder[T]`, and when the pipeline comes from config `T` is only known at runtime, where no implicit can be
summoned for it. The consequence is that a spark stage's input and output are almost always `DataFrame`: the
type computations of §7 are mostly needed by the generic stages of `h8io.stages` lib, not by the spark ones.

## 4. Pipelines

**A pipeline is a `Stage`.** `Stage ~> Stage` is itself a `Stage`, so there is no separate "pipeline"
concept to model — in config as in code, a pipeline is just a stage that happens to be built out of
others. Two spellings, and whatever consumes them cannot tell the difference:

- a **single stage** — one tagged node;
- a **chain** — a list of tagged nodes, folded left with `~>` in list order.

And it nests: a stage's own configuration can take **a pipeline as a parameter**. `cycles.Loop` and
`cycles.Repeat` each wrap one inner stage; `operators.And` and `operators.Or` each take two branches. The
parameter positions in those configs accept exactly the two spellings above, so the pipeline decoder is
recursive and reused wherever a stage asks for a stage.

## 5. Tags

Every stage type comes with a **`StageBuilder`**, the object that turns one config node into the `Stage`
itself. **The tag selects the builder.** `Tagged` resolves a node to that tag plus the node that is left,
accepting four spellings: a node tag from the source (`!kind {…}` in YAML, `@kind {…}` in Psi), a `_` key
holding the tag (`{_: kind, …}`, stripped from the map), a single-key map (`{kind: {…}}`), or a bare scalar
(`kind`, with `Node.Null` as the config). A map with no tag, no `_` and several keys is `AmbiguousMap`; a
`_` that is not a scalar is `NonScalarTag`.

Three of those four spellings are a crutch, and deliberately so: only cfg's YAML and Psi loaders produce
real node tags. The HOCON/JSON loader goes through Typesafe Config, which has no notion of a tag at all —
every node it builds has `tag == None`, unconditionally. So for HOCON the tag has to be carved out of
ordinary map structure, which is what `_` and the single-key map do.

A tag is either **the FQN of a stage class** or **an alias** for one. Aliases depend on the domain the
pipeline is written for, so each domain module brings its own table (spark has one for its stages); core
only knows how to consult the tables it is given and expand an alias into an FQN.

## 6. Builders and their lookup

**The builder is the stage's companion object**, in the same module as the stage:
`object Filter extends StageBuilder[...]` next to `class Filter`. It parses its own parameters from the
config node. **core finds it by reflection**, from the FQN: it loads the companion class (`<FQN>$`) and takes
its `MODULE$`, which both 2.13 and 3.9 emit for an object. Reflection is used only to *find* the builder —
core never calls a stage's constructor or reads its parameters itself. Three things this lookup has to get
right:

1. **Check before initializing.** The tag comes from config, which is effectively outside input, and
   `Class.forName(name)` with initialization runs the static initializer of whatever class it names. Load
   with `initialize = false`, check `classOf[StageBuilder[?]].isAssignableFrom(cls)`, and only then read
   `MODULE$`. A class that fails the check is a `NodeError` on the tag's node, not an exception.
2. **The right classloader.** Under Spark, user jars (`--jars`, `spark.jars`) are loaded by a classloader of
   their own, which the one that loaded xi-core cannot see into. Use the thread's context classloader, or
   one passed in explicitly — not `getClass.getClassLoader`.
3. **A precise error.** A stage class without a companion object has no `<FQN>$`, and the error has to say
   that the class has no builder, not that the class was not found.

## 7. Runtime types

Types are carried at runtime, because the pipeline shape is only known once the config is read. The two
drafts that exist in code:

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
reports a `NodeError` against that node's `Location`.

That is the argument for izumi-reflect over `h8io.reflect`: `LightTypeTag` has `typeArgs`, `withoutArgs`
and `combine` for exactly this taking-apart and rebuilding, alongside `<:<`/`=:=`, while
`h8io.reflect.Type` exposes only the comparisons. Treat the `h8io.reflect.Type` in
`StageBuilder`/`StageSpec` as provisional (§9).

## 8. Errors

Errors are accumulated rather than thrown, so that a bad config reports every problem with its source
`Location` at once. Each is a cfg `NodeError`, tied to the node it is about. `StageBuilder` returns
`Validated[NodeError, _]`, which used to be cfg-schema's `CfgValue`; whether it stays that is open (§9).

## 9. Open questions

- **What accumulates errors.** cfg-schema gave `CfgValue`, an alias of cats' `Validated`; cfg 0.4.x has
  neither. cats-core is still on the classpath, but only transitively, through the `io.h8 %% stages`
  umbrella artifact and its `stages-cats`. Options: depend on cats-core directly, or a small accumulating
  type of xi's own.
- **What replaces cfg-schema's `Decoder`** for the builders' parameters: each builder reading `Node` by
  hand, or a small decoding layer in core that builders share.
- **izumi-reflect or `h8io.reflect`.** izumi is on the classpath today only through `reflect` 0.0.6
  (izumi-reflect 3.0.11), so moving to izumi means depending on it directly and dropping `io.h8 %% reflect`.
  What that gives up is reflect's special-casing (primitive vs boxed, `Any`/`AnyVal`/`AnyRef`/`Nothing`/
  `Null`); whether xi needs it is to be found out from a real pipeline, not settled in advance.
- **The `StageBuilder` signature**: the error type parameter `E`, how a builder receives what it needs to
  build nested pipelines (core's decoder, the alias tables), and whether the alias tables are passed in or
  discovered.
- **The alias table**: its format and how a domain module hands it to core.
- **FQN tags in Psi** need backticks — ``@`h8io.xi.spark.Filter` `` — because a dot does not fit
  `Id.SafeKeyPattern`. Aliases make that rare; whether anything more is needed is open.
- **The documentation site with a 2.13-only module.** `pages` cannot depend on spark on the 3.9 row, and
  `dependsOn` does not vary with the Scala version: either the site is built for 2.13 only, or the Scala 3
  API is built separately, without spark.

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
