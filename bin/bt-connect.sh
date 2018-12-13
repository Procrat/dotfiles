#!/bin/bash
#
# Connects with some known bluetooth devices, chosen with dmenu.

set -euo pipefail


declare -A MAC_ADDRESSES


function enable_bluetooth() {
    rfkill unblock bluetooth

    if ! systemctl is-active bluetooth >/dev/null; then
        sudo systemctl start bluetooth
    fi

    bluetoothctl <<<'power on'
}

function populate_mac_addresses() {
    readarray -t device_lines < <(bluetoothctl <<<'devices' | \
        grep '^Device ' | \
        cut -d ' ' -f 2-)

    for line in "${device_lines[@]}"; do
        read -rs mac_address device_name <<<"$line"
        MAC_ADDRESSES[$device_name]=$mac_address
    done
}


enable_bluetooth

populate_mac_addresses

declare -a DEVICES=("${!MAC_ADDRESSES[@]}")
device=$(printf '%s\n' "${DEVICES[@]}" | mydmenu bluetooth-devices -p device:)
mac_address="${MAC_ADDRESSES[$device]}"

bluetoothctl <<<"connect $mac_address"
