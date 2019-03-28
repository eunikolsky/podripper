#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

STREAM_URL="http://rcmp.me:8000/stream"
# The expected duration of the stream, minimum duration to rip everything, maximum duration to stop somewhere.
DURATION_SEC="30"
# The sleep duration between rip retries.
RETRY_SEC="10"
# The output directory for the rips.
OUTPUT_DIR="$HOME/.podripper"

[[ -d "$OUTPUT_DIR" ]] || mkdir "$OUTPUT_DIR"
cd "$OUTPUT_DIR"

# at the start, figure out the duration until which keep on ripping the stream
END_TIMESTAMP="$( date -d "+ ${DURATION_SEC} seconds" '+%s' )"

while (( $( date '+%s' ) < "$END_TIMESTAMP" )); do
  echo "*** starting ripping at $( date )"
  streamripper "$STREAM_URL" -s -r -R 3 -a -A -o version -t -m 5 -M 1000 -l "$DURATION_SEC"
  sleep "$RETRY_SEC"
done

# vim: et ts=2 sw=2
