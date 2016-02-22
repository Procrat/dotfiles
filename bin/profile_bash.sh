#!/usr/bin/env bash
#
# Writes profiling info (resp. accumulated info) to $$.timings (resp.
# $$.acc_timings) in working directory. The concrete filenames of both files
# are written to stderr.
#
# This script assumes file descriptor 3 is not messed with in the profiled
# script, nor with the xtrace option of bash.

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

paste \
    <(cut -f1 $$.timings) \
    <(cat /tmp/bash-profile-$$.log | \
        sed 's/^+\+ //' | \
        sed 's/^\(local\s\|declare\s\|\S\+=\).*/[[assignment]]/') | \
    tail -n +2 | \
    awk '{
        cumtime[$2] += $1
        count[$2] += 1
    }
    END {
        printf("cumtime\tcalls\tavg.time\tcommand\n")
        for (key in cumtime) {
            printf("%f\t%d\t%f\t%s\n", cumtime[key], count[key],
                   cumtime[key] / count[key], key)
        }
    }' | \
    sort -k1gr >$$.acc_timings

rm /tmp/bash-profile-$$.{tim,log}

echo $$.timings $$.acc_timings >&2
