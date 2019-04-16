#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

# Config name should be passed as the single parameter.
CONF_NAME="${1:?no config name}.conf"

# The configuration file should set the following variables:
# * `STREAM_URL`
# * `DURATION_SEC` -- The expected duration of the stream, minimum duration to rip everything, maximum duration to stop somewhere.
# * `RETRY_SEC` -- The sleep duration between rip retries.
# * `RIP_DIR_NAME` -- The base name of the rip directory, prepended to today's date.
# * `POD_ARTIST`, `POD_ALBUM` -- ID3 tag information.

# The directory of the script, to locate extra resources.
SCRIPT_DIR="$( dirname "$0" )"

set -o allexport
# shellcheck source=/dev/null
. "$SCRIPT_DIR/$CONF_NAME"
set +o allexport

# The base output directory for the rips.
BASE_OUTPUT_DIR="$HOME/.podripper"
# The output directory for the current rip.
RIP_OUTPUT_DIR="$BASE_OUTPUT_DIR/${RIP_DIR_NAME}_$( date '+%F' )"
GIST_ID="c1ddfca162a9e4f648fcae697a827463"

[[ -d "$BASE_OUTPUT_DIR" ]] || mkdir "$BASE_OUTPUT_DIR"
cd "$BASE_OUTPUT_DIR"

[[ -d "$RIP_OUTPUT_DIR" ]] || mkdir "$RIP_OUTPUT_DIR"

# at the start, figure out the duration until which keep on ripping the stream
END_TIMESTAMP="$( date -d "+ ${DURATION_SEC} seconds" '+%s' )"

while (( $( date '+%s' ) < "$END_TIMESTAMP" )); do
  echo "*** starting ripping at $( date )"
  streamripper "$STREAM_URL" --quiet -d "$RIP_OUTPUT_DIR" -s -r -R 3 -a -A -o version -t -m 5 -M 1000 -l "$DURATION_SEC"

  # if we've run out of time, no need to sleep one more time at the end
  if (( $( date -d "+ ${RETRY_SEC} seconds" '+%s' ) < "$END_TIMESTAMP" )); then
    sleep "$RETRY_SEC"
  else
    break
  fi
done

# after we've spent enough time ripping, reencode the files (to fix the mp3 headers and stuff) and upload all the files to Firefox Send if any
if [[ -n "$( ls -A "$RIP_OUTPUT_DIR" )" ]]; then
  echo "*** reencoding files at $( date )"
  ENC_RIP_OUTPUT_DIR="${RIP_OUTPUT_DIR}_fix"
  # we don't expect the directory to exist, it should have been cleaned up before
  mkdir "$ENC_RIP_OUTPUT_DIR"

  year="$( date '+%Y' )"
  for rip in "$RIP_OUTPUT_DIR"/*.mp3; do
    pod_title="$( echo "$rip" | sed -nE 's/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\1-\2-\3 \4:\5:\6/p' )"
    lame --quiet --id3v2-only --tt "$pod_title" --ta "$POD_ARTIST" --tl "$POD_ALBUM" --ty "$year" --tg Podcast -V4 "$rip" "$ENC_RIP_OUTPUT_DIR/$( basename "$rip" )"
  done

  upload_retry_count=5
  upload_retry_sec=30

  while (( upload_retry_count > 0 )); do
    echo "*** uploading files at $( date )"
    if SHARE_LINK="$( ffsend --no-interact --yes --quiet upload "$ENC_RIP_OUTPUT_DIR" )"; then
      break
    else
      sleep "$upload_retry_sec"
      upload_retry_sec=$(( upload_retry_sec * 2 ))
      upload_retry_count=$(( upload_retry_count - 1 ))
    fi
  done

  if [[ -z "$SHARE_LINK" ]]; then
    echo "Failed to upload the files at $( date ); aborting" >&2
    exit 1
  fi

  echo "Link: $SHARE_LINK"

  # append the link to the gist
  {
    gist -r "$GIST_ID"
    echo "$( date ) :: $SHARE_LINK"
  } | gist -u "$GIST_ID"

  trash-put "$ENC_RIP_OUTPUT_DIR"
else
  echo "no files in $RIP_OUTPUT_DIR; nothing to upload"
fi

echo "*** removing $RIP_OUTPUT_DIR at $( date )"
trash-put "$RIP_OUTPUT_DIR"

# vim: et ts=2 sw=2
