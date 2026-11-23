# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`xi` (Xi Query Language) is a Scala library that turns [cfg](https://cfg.h8.io/) configuration into
[stages](https://stages.h8.io/) pipelines: it reads a cfg node tree and builds stages wired together, so a
pipeline can be described in a config file rather than in code. It is early — nothing is published to Maven
Central yet, and the only library code is two drafts, `StageBuilder` and `StageSpec`: the rest of the first
draft (`Tagged`, built on cfg-schema) was removed when cfg 0.4.x dropped cfg-schema.

## The design

The design lives in **`design/design.md`**, the design notes. Read the relevant sections before changing
anything they describe: §2 lists what is settled, §9 what is open, and §10 logs how each decision was reached,
so a question already argued through is not reopened. A design change is decided with the user before it is
coded, and recorded there, in its section and in §10.

In short, as of now: two modules, **core** (domain-agnostic: tags, builder lookup, chains, nested pipelines,
runtime types, error accumulation; 2.13 and 3.9) and **spark** (simple stages bound to Spark on `DataFrame`,
each with its builder as the companion object; 2.13 only). A tag is a stage class's FQN or a domain alias,
and core finds the builder by reflection — with the class checked before it is initialized, through the
context classloader.

## Commands

```bash
./test.sh                      # everything CI runs: fmt check, cross-build, coverage gate, docs, site
sbt +testFull                  # cross-build the tests only; plain `test` skips what already passed
sbt "testOnly h8io.xi.SomeTest"
sbt "testOnly *SomeTest -- -z \"some example\""   # one example by name substring
sbt "scalafmtAll; scalafmtSbt" # format (test.sh only *checks*, it does not fix)
sbt pages/mdoc                 # compile the docs' `scala mdoc` blocks, into pages/target/mdoc
(cd pages && npm ci && npm run build)  # render pages/target/mdoc with Docusaurus, into pages/target/site
(cd pages && npm start)        # live preview; pair with `sbt "pages/mdoc --watch"`
```

`.sbtopts` sets `-batch`, so an `sbt` invocation always needs its commands on the command line; there is
no interactive shell to fall back into.

The build is on sbt 2, which differs from sbt 1 in ways that matter here:
- `sbt` starts a thin client that keeps a server running in the background. The client takes one command line, so
  several commands as separate arguments fail to parse; scripts run `sbt --server`, which works like sbt 1.
- `test` is incremental: it skips suites that passed before and have not changed since, even after `clean`. `testFull`
  runs everything.
- Task results, compilation included, are cached on disk machine-wide (`~/.cache/sbt`). A cached compilation bypasses
  the scoverage compiler plugin, so its instrumentation data is never written; `cleanFull` empties that cache (and
  `target`) and is how `test.sh` starts. CI starts with an empty cache anyway.
- Bare settings in `build.sbt` apply to every project, which is what `ThisBuild` used to be for. sbt-dynver's keys are
  the exception: the plugin reads them from `ThisBuild`.
- Every project's `target` lives under the root's `target/out`. `pages` writes mdoc and unidoc output to
  `pages/target` instead, where Docusaurus and `release.sh` read it; `pages/clean` empties it, `cleanFull` does not.

`+` prefixes cross-build a command over Scala 2.13.18 and 3.9.0 (LTS), through `crossScalaVersions`, not
`projectMatrix`. 2.13 is the main version and the priority, being the one native to current Spark. A bare `sbt test`
only exercises 2.13, and breakage hides in Scala 3 — cross-build before claiming a change compiles. Both share one
source tree, written in Scala 2 style that Scala 3 also accepts; idiomatic Scala 3 (`enum`, `extension`,
version-specific source directories) is for later. Every branch on the Scala version in `build.sbt` lists the
versions it knows and ends in `unsupportedScala`, so a new version fails the build until each one has been looked at.

## Build layout

Two sbt projects in `build.sbt`:

- **root** (`xi`, `src/`) — the library. Enables `ScoverageSummaryPlugin`.
- **pages** (`xi-pages`, `pages/`) — the documentation site. `publish / skip`, depends on root, enables
  `ScalaUnidocPlugin` + `MdocPlugin`. Two halves: sbt runs mdoc over `docs/` and unidoc, both into
  `pages/target`, then Docusaurus (`pages/package.json`, Node ≥ 20) renders mdoc's output. The navigation is
  `pages/sidebars.js` — a new page must be listed there. Docs are CommonMark, not MDX
  (`markdown.format: 'detect'`), and broken links fail the build. The navbar's API dropdown links to the
  unidoc output that `release.sh` copies in beside the site, one per Scala version (`api/scala-3`,
  `api/scala-2.13`) and listed there by hand, through `pathname://` so the link checker
  leaves it alone.

Common settings are bare, so they apply to both projects; `project/Dependencies.scala` holds versions. `build.sbt`
imports `h8io.sbt.dependencies.*` from the sbt-dependencies plugin, which is what makes
`libraryDependencies ++= TestBundle % Test` work on a `Seq[ModuleID]`.

## Upstream projects — read them, they are checked out locally

xi is a thin layer over three sibling libraries by the same author. Their sources are on this machine, and
consulting them directly is the normal way to answer an API question here — the published scaladoc lags and
these libraries change under xi.

| Repo | Path | What xi uses from it | Pinned in `Dependencies.scala` |
| --- | --- | --- | --- |
| [stages](https://stages.h8.io/) | `../stages` | `Stage[-I, +O, +E]`, `~>`, `Yield`, `Evolution`, `Status`; `lib/` has ready-made stages | `stages` 0.1.0 |
| [cfg](https://cfg.h8.io/) | `../cfg` | `Node` tree, `Id`, `Location`, `NodeError`, `CfgError` | `cfg` 0.4.1 |
| reflect (on the way out) | `../reflect` | `Type[T]` — runtime types with `<:<` / `=:=` | `reflect` 0.0.6 |

**Check the pin before trusting local source.** `../stages` at v0.1.0 and `../cfg` at v0.4.1 currently
match what `Dependencies.scala` asks for, but they drift — these libraries move under xi, and a local
checkout is normally the *newer* API. Bumping a pin is its own commit (cfg 0.0.12 and stages 0.0.22 both
arrived that way), so the two can be a release apart at any moment.

`reflect` is pinned at 0.0.6, which matches `../reflect`, but it is still on its way out rather than a
dependency to build on. Up to 0.0.4 it was built on `scala.reflect.runtime.universe` and carried variance
machinery (`Variant`, `Covariant`, `Contravariant`), which has no Scala 3 build; 0.0.5 rewrote it over izumi's
`LightTypeTag` with primitive/boxed pairs and `Any`/`AnyVal`/`AnyRef`/`Nothing`/`Null` special-cased, and 0.0.6
publishes that for Scala 3 too. Two different libraries under one name — and once it is mostly a wrapper over
izumi, xi may as well use izumi itself.

What that gives up is the special-casing 0.0.5 added — primitive-vs-boxed above all, and the
`Any`/`AnyVal`/`AnyRef`/`Nothing`/`Null` cases. Whether xi needs any of it is genuinely open: a wrapper
over `LightTypeTag` may end up back here for exactly those edge cases, or the question may never come up.
Don't settle it in advance, and don't write the wrapper on speculation — let a real pipeline that needs it
be the thing that puts it there.

`../cfg` has its own CLAUDE.md; all three follow the same build conventions as this repo.

## Constraints worth knowing before you edit

**Coverage is gated at 100%.** `coverageSummary{Stmt,Branch}{Low,High}Threshold := 100`, and
`+coverageSummaryCheck` in `test.sh` fails the build below that. New code needs tests covering every
statement and branch, or the build goes red. The check must come right after `+coverageSummary` and
`+coverageAggregate` — a `+` command runs per cross-build row, so a check placed earlier fails on the first
row and leaves the remaining rows' reports unwritten — and before `pages/mdoc`, which recompiles the
library with coverage still on and wipes the measurement files.

**Warnings are errors** on every version: `-Xfatal-warnings` plus `-Xlint:_`, `-Wunused:_`, `-Wdead-code` on 2.13,
`-Werror` with `-Wall` on 3. An unused import or a non-exhaustive match fails compilation everywhere. Discarded
values are warned about too (`-Wvalue-discard`, `-Wnonunit-statement`). Tests and
the docs' mdoc snippets silence those two through `-Wconf` (`lenientScalacOptions` in `build.sbt`): a ScalaMock
expectation, a ScalaTest assertion or an expression shown with its result is a statement whose value is dropped on
purpose. The one other exception is Scala 3's notice that a trailing `_` for eta-expansion is unnecessary — Scala 2
needs it.

**Sources are `-Xsource:3`.** Write `import cats.syntax.all.*`, `f(args*)`, `Node.IScalar[?]` — the Scala 3
spellings — not the 2.13 ones. scalafmt is pinned to the `scala213source3` dialect with
`fatalWarnings: true`, `maxColumn: 120`.

## Testing conventions

ScalaTest `AnyFlatSpec` with `Matchers`; ScalaMock's `MockFactory` for cfg's `Node` traits, where
expectations are set with the `(() => node.tag).expects()` arity-0 form. `cfg` does not publish its test
sources, so a helper such as a mock `Location` is copied here rather than imported.

Under Scala 3 ScalaMock cannot mock Java classes, so an opaque value is `new AnyRef`, not `mock[AnyRef]`; and a direct
call of an untyped polymorphic singleton from stages, such as `Identity(x)`, is an ambiguous overload, so name the
type: `Identity[Int](x)`.

## Release and CI

GitHub Actions delegates entirely to reusable workflows in `h8io/gha` at `@v7` (which installs Node.js for
the Docusaurus build): `test.yaml` on PRs to
`main`, `snapshot.yaml` on dispatch, `release.yaml` on a `v[0-9]+.[0-9]+.[0-9]+` tag (which also deploys
the site via `publish-pages: true`). Version numbers come from sbt-dynver — from the git tag, never from a
literal in `build.sbt`. `release.sh` deliberately builds the site *before* `ci-release`, because
publishing to Maven Central cannot be undone and everything that can still fail belongs in front of it —
which is why sbt runs twice there, with Docusaurus in between.

## Repository habits

Commit subjects are imperative and specific ("Take Tagged over from cfg"), with a wrapped body that
explains *why* and records what was measured or checked. Dependency bumps arrive from Scala Steward;
reformat-only commits go into `.git-blame-ignore-revs`.
