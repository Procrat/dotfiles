#!/usr/bin/env bash
#
# Writes profiling info to $$.timings in working directory.
# The concrete filename of $$.timings is written to stderr.
# Assumes file descriptor 3 is not messed with in the profiled script.

exec 3>&2 2> >(tee /tmp/bash-profile-$$.log | \
               sed -u 's/^.*$/now/' | \
               date -f - +%s.%N >/tmp/bash-profile-$$.tim)
set -x

. "$@"

set +x
exec 2>&3 3>&-

paste <(
    while read tim ;do
        crt=000000000$((${tim//.}-10#0$last))
        printf "%12.9f\n" ${crt:0:${#crt}-9}.${crt:${#crt}-9}
        last=${tim//.}
    done < /tmp/bash-profile-$$.tim
) /tmp/bash-profile-$$.log > $$.timings

rm /tmp/bash-profile-$$.{tim,log}
echo $$.timings >&2
