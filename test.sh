#!/bin/bash

set -euxo pipefail

sbt scalafmtSbtCheck scalafmtCheckAll +clean +coverage +test +coverageAggregate +coverageSummary
