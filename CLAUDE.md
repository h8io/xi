# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`xi` (Xi Query Language) is a Scala library that builds [stages](https://stages.h8.io/) pipelines out of
configuration: it reads a [cfg](https://cfg.h8.io/) node tree and turns it into stages wired together, so
a pipeline can be described in a config file rather than in code. It is early — nothing is published to
Maven Central yet, and `Factorial` is scaffolding left from the project template rather than real API.

## The design

> The code in `src/` is a **very rough draft**. `StageBuilder`, `StageSpec` and `Tagged` sketch a
> direction; none of the signatures are settled. Read this section for intent, not as a contract, and
> expect to change what is there rather than work around it.

**A pipeline is a `Stage`.** `Stage ~> Stage` is itself a `Stage`, so there is no separate "pipeline"
concept to model — in config as in code, a pipeline is just a stage that happens to be built out of
others. Two spellings, and whatever consumes them cannot tell the difference:

- a **single stage** — one tagged node;
- a **chain** — a list of tagged nodes, folded left with `~>` in list order.

And it nests: a stage's own configuration can take **a pipeline as a parameter**. `cycles.Loop` and
`cycles.Repeat` each wrap one inner stage; `operators.And` and `operators.Or` each take two branches. The
parameter positions in those configs accept exactly the two spellings above, so the pipeline decoder is
recursive and reused wherever a stage asks for a stage.

Every stage type comes with a **`StageBuilder`**, the object that turns one config node into the `Stage`
itself. **The tag selects the builder.** `Tagged` resolves a node to that tag plus the node that is left,
accepting four spellings: a node tag from the source (`!kind {…}`), a `_` key holding the tag
(`{_: kind, …}`, stripped from the map), a single-key map (`{kind: {…}}`), or a bare scalar (`kind`, with
`Node.Null` as the config). A map with no tag, no `_` and several keys is `AmbiguousMap`; a `_` that is
not a scalar is `NonScalarTag`.

Three of those four spellings are a crutch, and deliberately so: only cfg's YAML backend produces real
node tags. The HOCON/JSON backend goes through Typesafe Config, which has no notion of a tag at all —
`MapImpl.tag`, `SeqImpl.tag` and every scalar it builds return `None`, unconditionally. So for HOCON the
tag has to be carved out of ordinary map structure, which is what `_` and the single-key map do.

A tag is either **an FQN of a class** or **an alias** for one. Aliases will depend on the domain the
pipeline is written for, so what they are and where the table of them lives is deliberately open — it gets
settled per concrete implementation, not here.

Types are carried at runtime, because the pipeline shape is only known once the config is read:

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
a `StageSpec` for each, and derives its own output type from theirs. Errors are accumulated as
`Validated[NodeError, _]` (cfg's `CfgValue`) rather than thrown, so a bad config reports every problem
with its source `Location` at once.

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
`h8io.reflect.Type` exposes only the comparisons. izumi is also already on the classpath — cfg-schema
depends on izumi-reflect 3.0.9 and `SelectiveDecoder` requires a `Tag` — so `io.h8 %% reflect` would just
go away. Treat the `h8io.reflect.Type` in `StageBuilder`/`StageSpec` as provisional.

Most of this is still intent, not code: `StageBuilder`'s companion is empty, there is no builder registry,
and nothing yet decodes either pipeline form or descends into a nested one. `Tagged` and `StageSpec` are
the parts that exist, and they are drafts too.

## Commands

```bash
./test.sh                      # everything CI runs: fmt check, cross-build, coverage gate, docs, site
sbt +test                      # cross-build the tests only
sbt "testOnly h8io.xi.cfg.TaggedDecoderTest"
sbt "testOnly *TaggedDecoderTest -- -z \"single key\""   # one example by name substring
sbt scalafmtAll scalafmtSbt    # format (test.sh only *checks*, it does not fix)
sbt pages/tlSite               # build the docs site into pages/target/docs/site
```

`.sbtopts` sets `-batch`, so an `sbt` invocation always needs its commands on the command line; there is
no interactive shell to fall back into.

`+` prefixes cross-build a command over Scala 2.13.18 and 2.12.21. A bare `sbt test` only exercises 2.13,
which is where most `-Xsource:3`-dependent breakage hides — cross-build before claiming a change compiles.

## Build layout

Two sbt projects in `build.sbt`:

- **root** (`xi`, `src/`) — the library. Enables `ScoverageSummaryPlugin`.
- **pages** (`xi-pages`, `pages/`) — the documentation site. `publish / skip`, depends on root, enables
  `ScalaUnidocPlugin` + `TypelevelSitePlugin`. Its markdown source lives in `docs/`, is run through mdoc
  and rendered by Laika, and `tlSiteApiUrl` points at the unidoc output that `release.sh` copies in
  beside it (`target/pages/api/scala-2.{12,13}`).

Common settings are `ThisBuild /` scoped; `project/Dependencies.scala` holds versions. `build.sbt`
imports `h8io.sbt.dependencies.*` from the sbt-dependencies plugin, which is what makes
`libraryDependencies ++= TestBundle % Test` work on a `Seq[ModuleID]`.

## Upstream projects — read them, they are checked out locally

xi is a thin layer over three sibling libraries by the same author. Their sources are on this machine, and
consulting them directly is the normal way to answer an API question here — the published scaladoc lags and
these libraries change under xi.

| Repo | Path | What xi uses from it | Pinned in `Dependencies.scala` |
| --- | --- | --- | --- |
| [stages](https://stages.h8.io/) | `../stages` | `Stage[-I, +O, +E]`, `~>`, `Yield`, `Evolution`, `Status`; `lib/` has ready-made stages | `stages` 0.0.22 |
| [cfg](https://cfg.h8.io/) | `../cfg` | `Node` tree, `Id`, `Location`, `NodeError`; `cfg-schema` for `Decoder`, `SelectiveDecoder`, `CfgValue`, `Property` | `cfg` / `cfg-schema` 0.0.12 |
| reflect (on the way out) | `../reflect` | `Type[T]` — runtime types with `<:<` / `=:=` | `reflect` 0.0.4 |

**Check the pin before trusting local source.** `../stages` at v0.0.22 and `../cfg` at v0.0.12 currently
match what `Dependencies.scala` asks for, but they drift — these libraries move under xi, and a local
checkout is normally the *newer* API. Bumping a pin is its own commit (cfg 0.0.12 and stages 0.0.22 both
arrived that way), so the two can be a release apart at any moment.

`reflect` is the one that is genuinely out of step, and the reason it is being dropped rather than bumped.
The pinned
0.0.4 is built on `scala.reflect.runtime.universe` and carries variance machinery (`Variant`, `Covariant`,
`Contravariant`); the local v0.0.5 is a rewrite over izumi's `LightTypeTag` with primitive/boxed pairs and
`Any`/`AnyVal`/`AnyRef`/`Nothing`/`Null` special-cased. Two different libraries under one name — and once
the newer one is mostly a wrapper over izumi, xi may as well use izumi itself.

What that gives up is the special-casing 0.0.5 added — primitive-vs-boxed above all, and the
`Any`/`AnyVal`/`AnyRef`/`Nothing`/`Null` cases. Whether xi needs any of it is genuinely open: a wrapper
over `LightTypeTag` may end up back here for exactly those edge cases, or the question may never come up.
Don't settle it in advance, and don't write the wrapper on speculation — let a real pipeline that needs it
be the thing that puts it there.

`../cfg` has its own CLAUDE.md; all three follow the same build conventions as this repo.

## Constraints worth knowing before you edit

**Coverage is gated at 100%.** `ThisBuild / coverageSummary{Stmt,Branch}{Low,High}Threshold := 100`, and
`+coverageSummaryCheck` in `test.sh` fails the build below that. New code needs tests covering every
statement and branch, or the build goes red. The check must stay *last* in the sbt command line, after
`+coverageSummary` and `+coverageAggregate` — a `+` command runs per cross-build row, so a check placed
earlier fails on the first row and leaves the remaining rows' reports unwritten.

**Warnings are errors.** `-Xfatal-warnings` plus `-Xlint:_`, `-Wunused:_`, `-Wdead-code` on 2.13. An
unused import or a non-exhaustive match fails compilation, on both Scala versions.

**Sources are `-Xsource:3`.** Write `import cats.syntax.all.*`, `f(args*)`, `Node.IScalar[?]` — the Scala 3
spellings — not the 2.13 ones. scalafmt is pinned to the `scala213source3` dialect with
`fatalWarnings: true`, `maxColumn: 120`.

## Testing conventions

ScalaTest `AnyFlatSpec` with `Matchers`; ScalaMock's `MockFactory` for cfg's `Node` traits, where
expectations are set with the `(() => node.tag).expects()` arity-0 form. `cfg` does not publish its test
sources, so helpers like `src/test/scala/h8io/xi/cfg/testutil/MockLocation.scala` are copied here rather
than imported. Decoder results are `Validated`, compared with cats syntax (`shouldBe Tagged(...).valid` /
`.invalid`).

## Release and CI

GitHub Actions delegates entirely to reusable workflows in `h8io/gha` at `@v6`: `test.yaml` on PRs to
`main`, `snapshot.yaml` on dispatch, `release.yaml` on a `v[0-9]+.[0-9]+.[0-9]+` tag (which also deploys
the site via `publish-pages: true`). Version numbers come from sbt-dynver — from the git tag, never from a
literal in `build.sbt`. `release.sh` deliberately builds the site *before* `ci-release`, because
publishing to Maven Central cannot be undone and everything that can still fail belongs in front of it.

## Repository habits

Commit subjects are imperative and specific ("Take Tagged over from cfg"), with a wrapped body that
explains *why* and records what was measured or checked. Dependency bumps arrive from Scala Steward;
reformat-only commits go into `.git-blame-ignore-revs`.
