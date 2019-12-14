#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

# These environment variables can override the default values for debugging:
# * `END_TIMESTAMP` -- when to stop recording.

# Config name should be passed as the single parameter.
CONF_NAME="${1:?no config name}.conf"

# The configuration file should set the following variables:
# * `STREAM_URL`
# * `DURATION_SEC` -- The expected duration of the stream, minimum duration to rip everything, maximum duration to stop somewhere.
# * `RETRY_SEC` -- The sleep duration between rip retries.
# * `RIP_DIR_NAME` -- The base name of the rip directory
# * `POD_ARTIST`, `POD_ALBUM` -- ID3 tag information.

# The directory of the script, to locate extra resources.
# note: https://mywiki.wooledge.org/BashFAQ/028
SCRIPT_DIR="$( dirname "$( realpath -s "$0" )" )"

set -o allexport
# shellcheck source=/dev/null
. "$SCRIPT_DIR/$CONF_NAME"
set +o allexport

# The base output directory for the rips.
BASE_OUTPUT_DIR="/srv/http"
ENC_RIP_OUTPUT_DIR="$BASE_OUTPUT_DIR/$RIP_DIR_NAME"
# The output directory for the current rip.
RIP_OUTPUT_DIR="$ENC_RIP_OUTPUT_DIR/incomplete/"

[[ -d "$RIP_OUTPUT_DIR" ]] || mkdir -p "$RIP_OUTPUT_DIR"

cd "$BASE_OUTPUT_DIR"

# at the start, figure out the duration until which keep on ripping the stream
END_TIMESTAMP="${END_TIMESTAMP:-$( date -d "+ ${DURATION_SEC} seconds" '+%s' )}"

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

# after we've spent enough time ripping, reencode the files (to fix the mp3 headers and stuff)
if [[ -n "$( ls -A "$RIP_OUTPUT_DIR" )" ]]; then
  echo "*** reencoding files at $( date )"

  year="$( date '+%Y' )"
  for rip in "$RIP_OUTPUT_DIR"/*.mp3; do
    pod_title="$( echo "$rip" | sed -nE 's/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\1-\2-\3 \4:\5:\6/p' )"
    if ! ffmpeg -hide_banner -i "$rip" -vn -v warning -codec:a libmp3lame -q:a 4 -metadata title="$pod_title" -metadata artist="$POD_ARTIST" -metadata album="$POD_ALBUM" -metadata date="$year" -metadata genre=Podcast "$ENC_RIP_OUTPUT_DIR/$( basename -s .mp3 "$rip" )_enc.mp3"; then
      echo "reencoding $rip failed; moving the source"
      mv "$rip" "$ENC_RIP_OUTPUT_DIR/$( basename -s .mp3 "$rip" )_src.mp3"
    else
      rm -f "$rip"
    fi
  done

  # clean up after `streamripper`
  rm -f "$RIP_OUTPUT_DIR"/*.cue
else
  echo "no files in $RIP_OUTPUT_DIR"
fi

# finally, we should update the RSS feed
echo "*** generating the RSS feed at $( date )"
"$SCRIPT_DIR/rss.zsh" "$ENC_RIP_OUTPUT_DIR" > "$BASE_OUTPUT_DIR/$RIP_DIR_NAME.rss"

# vim: et ts=2 sw=2
