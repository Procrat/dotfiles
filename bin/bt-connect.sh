#!/bin/bash
#
# Connects with some known bluetooth devices, chosen with dmenu.

set -euo pipefail


declare -A DEVICES
DEVICES['Nude Super-M']='A0:E9:DB:5C:FD:44'
DEVICES['Zeus']='60:E3:27:09:12:DE'


mac_address() {
    local chosen_device=$(printf '%s\n' "${!DEVICES[@]}" | mydmenu)
    echo "${DEVICES[$chosen_device]}"
}

sudo systemctl start bluetooth
echo -e "power on\nconnect $(mac_address)\n" | \
    bluetoothctl