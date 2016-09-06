#!/bin/bash
#
# Finds the biggest directories in the working directory or in one or multiple
# given directories.

set -euo pipefail

if [[ $# -gt 0 ]]; then
    du --si -a --max-depth 1 "$@" | sort -h
else
    du --si -a --max-depth 1 . | sort -h
fi
