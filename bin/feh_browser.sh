#!/bin/bash
#
# Browse images in a folder starting with the specified file onwards.

set -euo pipefail
shopt -s nullglob

FEH_OPTIONS=(--info 'exiv2 %F')

if [[ -z "$1" || ! -f "$1" ]]; then
    echo "$0: First argument is not a file" >&2
    exit 1
fi

file=$(basename -- "$1")
dir=$(dirname -- "$1")
shift

cd -- "$dir"

arr=()
for i in *; do
    [[ -f $i ]] || continue
    arr+=("$i")
    [[ $i == "$file" ]] && c=$((${#arr[@]} - 1))
done

exec feh "${FEH_OPTIONS[@]}" "$@" -- "${arr[@]:c}" "${arr[@]:0:c}"
