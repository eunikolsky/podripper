#!/usr/bin/env zsh

set -euo pipefail
IFS=$'\n\t'

zmodload zsh/regex

MONTHS=(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec)

WORKDIR="${1:?no working directory}"
PODCAST_NAME="$( basename "$WORKDIR" )"

BASE_URL="https://example.org"

cd "$WORKDIR"

rss_item() {
    FILE="$1"

    local match
    if [[ "$FILE" -regex-match (20[[:digit:]]{2})_([[:digit:]]{2})_([[:digit:]]{2})_([[:digit:]]{2})_([[:digit:]]{2})_([[:digit:]]{2}) ]]; then
        # https://tools.ietf.org/html/rfc2822#section-3.3
        TZ="$( date '+%z' )"
        PUBDATE="${match[3]} ${MONTHS[$match[2]]} ${match[1]} ${match[4]}:${match[5]}:${match[6]} ${TZ}"
        TITLE="$PODCAST_NAME: $PUBDATE"
        FILE_URL="$BASE_URL/$PODCAST_NAME/$FILE"
        FILE_SIZE="$( stat -c %s "$FILE" )"

        echo "<item><title>$TITLE</title><guid isPermaLink="\""false"\"">$FILE</guid><description></description><pubDate>$PUBDATE</pubDate><enclosure url="\""$FILE_URL"\"" type="\""audio/mp3"\"" length="\""$FILE_SIZE"\"" /></item>"
    else
        >&2 echo "Can't parse date/time from filename: $FILE"
        return 1
    fi
}

PODCAST_TITLE=
PODCAST_DESCRIPTION=

rss_feed() {
    ITEMS="$1"

    echo "<rss version="\""2.0"\"" xmlns:atom="\""http://www.w3.org/2005/Atom"\""><channel><link>$BASE_URL</link><title>$PODCAST_TITLE</title><description>$PODCAST_DESCRIPTION</description><language>ru</language><generator>rss.zsh</generator><atom:link rel="\""self"\"" type="\""application/rss+xml"\"" href="\""$BASE_URL/$PODCAST_NAME.rss"\"" />"
    echo "$ITEMS"
    echo "</channel></rss>"
}

# automatic podcast detection based on the directory where this script is run
detect_podcast() {
    case "$PODCAST_NAME" in
        radiot )
            PODCAST_TITLE="Радио-Т Поток"
            PODCAST_DESCRIPTION="Запись потока Радио-Т"
            ;;

        rcmp )
            PODCAST_TITLE="Пиратский Канадский Лось и компания"
            PODCAST_DESCRIPTION="Запись потока RCMP"
            ;;

	80s80s )
	    PODCAST_TITLE="80s80s feed"
	    PODCAST_DESCRIPTION="Test RSS feed"
	    ;;

        * )
            >&2 echo "Unknown podcast directory: $DIR; exiting"
            exit 2
            ;;
    esac
}

generate_rss() {
    detect_podcast
    rss_feed "$(
        for f in *.mp3; do
            rss_item "$f"
        done
    )"
}

case "${2:-}" in
    "--also-serve" )
        case "$PODCAST_NAME" in
            radiot ) RSS_FILE=radiot.rss HTTP_PORT=50000 ;;
            rcmp ) RSS_FILE=rcmp.rss HTTP_PORT=50001 ;;
        esac
        generate_rss > "$RSS_FILE"
        python3 -m http.server "$HTTP_PORT"
        ;;

    "" )
        generate_rss
        ;;

    * )
        >&2 echo "Unknown option $1"
        exit 3
        ;;
esac
