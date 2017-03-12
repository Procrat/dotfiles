#!/bin/bash
#
# Connects with some known bluetooth devices, chosen with dmenu.

set -euo pipefail


declare -A MAC_ADDRESSES PULSE_SINKS
MAC_ADDRESSES['Nude Super-M']='A0:E9:DB:5C:FD:44'
MAC_ADDRESSES['Zeus']='60:E3:27:09:12:DE'
PULSE_SINKS['Nude Super-M']='bluez_sink.A0_E9_DB_5C_FD_44.a2dp_sink'
declare -a DEVICES=("${!MAC_ADDRESSES[@]}")

rfkill unblock bluetooth
systemctl is-active bluetooth >/dev/null || sudo systemctl start bluetooth

device=$(printf '%s\n' "${DEVICES[@]}" | mydmenu)
mac_address="${MAC_ADDRESSES[$device]}"
pulse_sink="${PULSE_SINKS[$device]:-}"

{
    echo 'power on'
    echo -e "connect $mac_address"
} | bluetoothctl -a

[[ -n "$pulse_sink" ]] && pactl set-default-sink "$pulse_sink"
