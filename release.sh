#!/bin/bash

set -euxo pipefail

sbt +clean +test ci-release
