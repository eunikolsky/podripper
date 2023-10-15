#!/usr/bin/env bash

STREAM_NAME="$1"

# The directory with the config files.
CONF_DIR="${CONF_DIR:-/usr/share/podripper}"
# Config name should be passed as the single parameter.
CONF_NAME="${STREAM_NAME:?no config name}.conf"

# The configuration file should set the following variables:
# * `STREAM_URL`
# * `DURATION_SEC` -- The expected duration of the stream, minimum duration to rip everything, maximum duration to stop somewhere.
# * `RETRY_SEC` -- The sleep duration between rip retries.
# * `RIP_DIR_NAME` -- The base name of the rip directory
# * `POD_ARTIST`, `POD_ALBUM` -- ID3 tag information.

# /var/lib/podripper/               # set from outside, e.g. by `systemd`
# +- $RIP_DIR_NAME                  # $RAW_RIP_DIR, local, raw recordings
# \- $DONE_BASE_DIR/$RIP_DIR_NAME   # $DONE_RIP_DIR, S3, converted recordings + RSS files

# TODO cleanup complete rips on S3 after a year?
# local raw rip `mp3`s are removed/moved in the reencoding cycle below

# the flag shows whether the live stream check has returned success since the start
# we don't need to ask it anymore after that
#STREAM_IS_LIVE=

#wait_for_stream() {
  # at the start, figure out the duration until which keep on waiting for the stream
  #END_TIMESTAMP="${END_TIMESTAMP:-$( "$DATE" -d "+ ${DURATION_SEC} seconds" '+%s' )}"

  #while (( $( "$DATE" '+%s' ) < "$END_TIMESTAMP" )); do
    # FIXME the `atp` support is hardcoded in the program because its live
    # stream check is more complicated and the stream URL needs to be extracted
    # from the status endpoint; implementing a DSL in `conf` files and shell
    # isn't easy, so this should be more easily done when the script is
    # rewritten in Haskell
    #if [[ "$STREAM_NAME" == atp ]]; then
      #STATUS="$(curl -sS https://atp.fm/livestream_status)"
      #echo "$STATUS"

      #if [[ -z "$STREAM_IS_LIVE" ]]; then
        ## no live stream yet
        #if jq -e .live <<< "$STATUS" >/dev/null; then
          #STREAM_IS_LIVE=1

          ## try to parse the stream url from the status response
          #ORIG_STREAM_URL="$STREAM_URL"
          #PLAYER="$( jq -r .player <<< "$STATUS" || true )"
          ## this overwrites the global variable
          #STREAM_URL="$( htmlq -a src 'audio source' <<< "$PLAYER" || true )"
          #echo "  0 stream url (audio source): $STREAM_URL"
          #[[ -z "$STREAM_URL" ]] && STREAM_URL="$( htmlq -a src audio <<< "$PLAYER" || true )"
          #echo "  1 stream url (audio): $STREAM_URL"
          #[[ -z "$STREAM_URL" ]] && STREAM_URL="$( sed -nE 's/.*"(http[^"]+)".*/\1/p' <<< "$PLAYER" || true )"
          #echo "  2 stream url (sed): $STREAM_URL"
          #[[ -z "$STREAM_URL" ]] && STREAM_URL="$ORIG_STREAM_URL"
          #echo "  3 stream url (original): $STREAM_URL"
        #fi
      #fi
    #else
      #STREAM_IS_LIVE=1
    #fi

    # if we've run out of time, no need to sleep one more time at the end
    #if (( $( "$DATE" -d "+ ${RETRY_SEC} seconds" '+%s' ) < "$END_TIMESTAMP" )); then
      #sleep "$RETRY_SEC"
    #else
      #break
    #fi
  #done
#}

rip() {
  #if [[ -n "$STREAM_IS_LIVE" ]]; then
    #_ || true
  #fi
}

#ensure_dirs
#wait_for_stream
#rip
#reencode_previous_rips
#reencode_rips
#update_rss

# vim: et ts=2 sw=2
