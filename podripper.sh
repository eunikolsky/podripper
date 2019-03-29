#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

STREAM_URL="${STREAM_URL:-http://rcmp.me:8000/stream}"
# The expected duration of the stream, minimum duration to rip everything, maximum duration to stop somewhere.
DURATION_SEC="${DURATION_SEC:-16200}"
# The sleep duration between rip retries.
RETRY_SEC="${RETRY_SEC:-60}"
# The base output directory for the rips.
BASE_OUTPUT_DIR="$HOME/.podripper"
# The output directory for the current rip.
RIP_OUTPUT_DIR="$BASE_OUTPUT_DIR/rcmp_$( date '+%F' )"
GIST_ID="c1ddfca162a9e4f648fcae697a827463"

[[ -d "$BASE_OUTPUT_DIR" ]] || mkdir "$BASE_OUTPUT_DIR"
cd "$BASE_OUTPUT_DIR"

[[ -d "$RIP_OUTPUT_DIR" ]] || mkdir "$RIP_OUTPUT_DIR"

# at the start, figure out the duration until which keep on ripping the stream
END_TIMESTAMP="$( date -d "+ ${DURATION_SEC} seconds" '+%s' )"

while (( $( date '+%s' ) < "$END_TIMESTAMP" )); do
  echo "*** starting ripping at $( date )"
  streamripper "$STREAM_URL" --quiet -d "$RIP_OUTPUT_DIR" -s -r -R 3 -a -A -o version -t -m 5 -M 1000 -l "$DURATION_SEC"
  sleep "$RETRY_SEC"
done

# after we've spent enough time ripping, upload all the files to Firefox Send if any
if [[ -z "$( ls -A "$RIP_OUTPUT_DIR" )" ]]; then
  echo "no files in $RIP_OUTPUT_DIR; nothing to upload"
  exit 1
fi

echo "*** uploading files at $( date )"
SHARE_LINK="$( ffsend --no-interact --yes --quiet upload "$RIP_OUTPUT_DIR" )"
echo "Link: $SHARE_LINK"

# append the link to the gist
{
  gist -r "$GIST_ID"
  echo "$( date ) :: $SHARE_LINK"
} | gist -u "$GIST_ID"

# vim: et ts=2 sw=2
