#!/usr/bin/env bash

STREAM_IS_LIVE=

wait_for_stream() {
  while (( $( "$DATE" '+%s' ) < "$END_TIMESTAMP" )); do
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
  done
}

# vim: et ts=2 sw=2
