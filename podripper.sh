#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n\t'

# These environment variables can override the default values for debugging:
# * `END_TIMESTAMP` -- when to stop recording.

# The directory with the config files.
CONF_DIR="${CONF_DIR:-/usr/share/podripper}"
# Config name should be passed as the single parameter.
CONF_NAME="${1:?no config name}.conf"

# The configuration file should set the following variables:
# * `STREAM_URL`
# * `DURATION_SEC` -- The expected duration of the stream, minimum duration to rip everything, maximum duration to stop somewhere.
# * `RETRY_SEC` -- The sleep duration between rip retries.
# * `RIP_DIR_NAME` -- The base name of the rip directory
# * `POD_ARTIST`, `POD_ALBUM` -- ID3 tag information.

set -o allexport
# shellcheck source=/dev/null
. "$CONF_DIR/$CONF_NAME"
set +o allexport

# /var/lib/podripper/               # set from outside, e.g. by `systemd`
# +- $RIP_DIR_NAME                  # $RAW_RIP_DIR, local, raw recordings
# \- $DONE_BASE_DIR/$RIP_DIR_NAME   # $DONE_RIP_DIR, S3, converted recordings + RSS files

# The base directory for complete rips; this should be mounted from S3.
DONE_BASE_DIR="complete"
# The output directory for reencoded and processed rips. (It is possible
# that it contains raw rips when `ffmpeg` fails to process them.)
DONE_RIP_DIR="$DONE_BASE_DIR/$RIP_DIR_NAME"
# The output directory for raw rips recorded by streamripper.
RAW_RIP_DIR="$RIP_DIR_NAME"

[[ -d "$RAW_RIP_DIR" ]] || mkdir -p "$RAW_RIP_DIR"
[[ -d "$DONE_RIP_DIR" ]] || mkdir -p "$DONE_RIP_DIR"

# TODO cleanup complete rips on S3 after a year?
# local raw rip `mp3`s are removed/moved in the reencoding cycle below

# at the start, figure out the duration until which keep on ripping the stream
END_TIMESTAMP="${END_TIMESTAMP:-$( date -d "+ ${DURATION_SEC} seconds" '+%s' )}"

while (( $( date '+%s' ) < "$END_TIMESTAMP" )); do
  streamripper "$STREAM_URL" --quiet -d "$RAW_RIP_DIR" -s -r -R 3 -a -A -o version -t -m 5 -M 1000 -l "$DURATION_SEC"

  # if we've run out of time, no need to sleep one more time at the end
  if (( $( date -d "+ ${RETRY_SEC} seconds" '+%s' ) < "$END_TIMESTAMP" )); then
    sleep "$RETRY_SEC"
  else
    break
  fi
done

# after we've spent enough time ripping, reencode the files (to fix the mp3 headers and stuff)
if [[ -n "$( ls -A "$RAW_RIP_DIR" )" ]]; then
  year="$( date '+%Y' )"
  for rip in "$RAW_RIP_DIR"/*.mp3; do
    pod_title="$( echo "$rip" | sed -nE 's/.*([0-9]{4})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2})_([0-9]{2}).*/\1-\2-\3 \4:\5:\6/p' )"
    if ! ffmpeg -hide_banner -i "$rip" -vn -v warning -codec:a libmp3lame -b:a 96k -metadata title="$pod_title" -metadata artist="$POD_ARTIST" -metadata album="$POD_ALBUM" -metadata date="$year" -metadata genre=Podcast "$DONE_RIP_DIR/$( basename -s .mp3 "$rip" )_enc.mp3"; then
      echo "reencoding $rip failed; moving the source"
      mv "$rip" "$DONE_RIP_DIR/$( basename -s .mp3 "$rip" )_src.mp3"
    else
      rm -f "$rip"
    fi
  done

  # clean up after `streamripper`
  rm -f "$RAW_RIP_DIR"/*.cue
else
  echo "no files in $RAW_RIP_DIR"
fi

# finally, we should update the RSS feed
cd "$DONE_BASE_DIR"
/usr/bin/rssgen-exe "${RIP_DIR_NAME}.rss"

# vim: et ts=2 sw=2
