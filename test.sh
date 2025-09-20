#!/bin/bash

set -euxo pipefail

if [[ -n "${CI:-}" ]]; then
  COVERAGE_AGGREGATE=
  COVERAGE_SUMMARY=+coverageSummary
else
  COVERAGE_AGGREGATE=+coverageAggregate
  COVERAGE_SUMMARY=
fi

sbt scalafmtSbtCheck scalafmtCheckAll +clean +coverage +test $COVERAGE_AGGREGATE $COVERAGE_SUMMARY
