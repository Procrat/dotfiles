#!/usr/bin/env bash
#
# Prints SSID and IP address of current wifi connection

set -euo pipefail

WIFI_DEVICE='wlp58s0'


# Shows SSID
iw dev "$WIFI_DEVICE" link | sed -rn 's/^\s*SSID: (.*)/\1/p'
# Shows IP address
ip addr show dev "$WIFI_DEVICE" | sed -rn 's/^\s*inet\s+([^/]*).*/\1/p'
