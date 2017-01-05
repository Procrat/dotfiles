#!/usr/bin/env bash
#
# Removes white noise while wearing headphones on Dell XPS 13
# Also fixes the strange issue where the stereo channels don't have the same
# volume.


amixer -c 0 set 'Headphone Mic Boost' 1%

volume=$(amixer get Master | grep -m1 -o '[0-9]\+%')
amixer set Master "$volume"
