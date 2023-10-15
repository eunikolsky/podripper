#!/usr/bin/env bash

# the flag shows whether the live stream check has returned success since the start
# we don't need to ask it anymore after that
STREAM_IS_LIVE=

wait_for_stream() {
  while (( $( "$DATE" '+%s' ) < "$END_TIMESTAMP" )); do
    # FIXME the `atp` support is hardcoded in the program because its live
    # stream check is more complicated and the stream URL needs to be extracted
    # from the status endpoint; implementing a DSL in `conf` files and shell
    # isn't easy, so this should be more easily done when the script is
    # rewritten in Haskell
    if [[ "$STREAM_NAME" == atp ]]; then
      STATUS="$(curl -sS https://atp.fm/livestream_status)"
      echo "$STATUS"

      if [[ -z "$STREAM_IS_LIVE" ]]; then
        # no live stream yet
        if jq -e .live <<< "$STATUS" >/dev/null; then
          STREAM_IS_LIVE=1

          # try to parse the stream url from the status response
          ORIG_STREAM_URL="$STREAM_URL"
          PLAYER="$( jq -r .player <<< "$STATUS" || true )"
          # this overwrites the global variable
          STREAM_URL="$( htmlq -a src 'audio source' <<< "$PLAYER" || true )"
          echo "  0 stream url (audio source): $STREAM_URL"
          [[ -z "$STREAM_URL" ]] && STREAM_URL="$( htmlq -a src audio <<< "$PLAYER" || true )"
          echo "  1 stream url (audio): $STREAM_URL"
          [[ -z "$STREAM_URL" ]] && STREAM_URL="$( sed -nE 's/.*"(http[^"]+)".*/\1/p' <<< "$PLAYER" || true )"
          echo "  2 stream url (sed): $STREAM_URL"
          [[ -z "$STREAM_URL" ]] && STREAM_URL="$ORIG_STREAM_URL"
          echo "  3 stream url (original): $STREAM_URL"
        fi
      fi
    else
      STREAM_IS_LIVE=1
    fi
  done
}

rip() {
  if [[ -n "$STREAM_IS_LIVE" ]]; then
  fi
}

# vim: et ts=2 sw=2
