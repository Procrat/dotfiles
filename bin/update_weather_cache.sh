#!/bin/bash

set -e

# Calculate the current Where On Earth ID.
WOEID=$(curl -sL 'https://weather.yahoo.com' \
        | grep -m1 'woeId' \
        | sed 's/.*woeId: \([0-9]*\).*/\1/')
# Get the forecast RSS for this WOEID
curl -sL "https://weather.yahooapis.com/forecastrss?w=${WOEID}&u=c" \
     -o ~/.cache/weather.xml
