#!/usr/bin/env bash

wait_for_stream() {
          # try to parse the stream url from the status response
          ORIG_STREAM_URL="$STREAM_URL"

          [[ -z "$STREAM_URL" ]] && STREAM_URL="$( htmlq -a src audio <<< "$PLAYER" || true )"
          echo "  1 stream url (audio): $STREAM_URL"
          [[ -z "$STREAM_URL" ]] && STREAM_URL="$( sed -nE 's/.*"(http[^"]+)".*/\1/p' <<< "$PLAYER" || true )"
          echo "  2 stream url (sed): $STREAM_URL"
          [[ -z "$STREAM_URL" ]] && STREAM_URL="$ORIG_STREAM_URL"
          echo "  3 stream url (original): $STREAM_URL"
}

# vim: et ts=2 sw=2
