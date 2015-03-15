#!/bin/bash
#
# Finds the biggest directories in the working directory or in one or multiple
# given directories.

set -e

if [[ $# -gt 0 ]]; then
    du -h --max-depth 1 "$@" | sort -h
else
    du -h --max-depth 1 . | sort -h
fi
