#!/usr/bin/env bash
#
# Allows for both control and view of cammie.

set -euo pipefail

CONTROL_URL='kelder.zeus.ugent.be/webcam/cgi/ptdc.cgi'
VIEW_URL='kelder.zeus.ugent.be/webcam/video/mjpg.cgi'

case "${1:-}" in
    klein) curl --data "command=set_pos&posX=0&posY=12"  "$CONTROL_URL" ;;
    groot) curl --data "command=set_pos&posX=50&posY=10" "$CONTROL_URL" ;;
    zetel) curl --data "command=set_pos&posX=50&posY=22" "$CONTROL_URL" ;;
esac > /dev/null

curl "$VIEW_URL" | mpv --no-correct-pts --fps 5 -
