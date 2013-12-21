#!/usr/bin/env bash

init_config()
{
    echo Creating $1... > /dev/stderr
    cat << __EOF__ > "$1"
HOST=localhost
SERVER="[::1]:8088"
BASE_PATH=api
CURL_EXTRA=-6
KEY=secret

DEBUG_OUTPUT=/dev/null
__EOF__
}

CONFIG="$(dirname "$0")/config.sh"
[ -f "$CONFIG" ] || init_config "$CONFIG"
echo "Using $CONFIG" > /dev/stderr
. "$CONFIG"

# REST interface

request()
{
    SUB_PATH="$1"
    JSON="$2"
    echo POST $SUB_PATH, json: > $DEBUG_OUTPUT
    echo $JSON > $DEBUG_OUTPUT
    echo . > $DEBUG_OUTPUT
    echo '###' Running curl: curl -g -X POST \
	"http://$SERVER/$BASE_PATH/$SUB_PATH" $EXTRA -v \
        -H "Content-Type: application/json" -H "Host: $HOST" \
	-d "$JSON" > $DEBUG_OUTPUT
    curl -g -X POST "http://$SERVER/$BASE_PATH/$SUB_PATH" $EXTRA -v \
        -H "Content-Type: application/json" -H "Host: $HOST" \
	-d "$JSON" 2> $DEBUG_OUTPUT
}

get()
{
    SUB_PATH="$1"
    [ "x$2" = "x" ] && { echo Parameters missing; exit 1; }
    PARAMS="$2&host=$HOST&key=$KEY"

    echo GET $SUB_PATH, params: > $DEBUG_OUTPUT
    echo $PARAMS > $DEBUG_OUTPUT
    echo . > $DEBUG_OUTPUT
    echo '###' Running curl: curl -g \
	"http://$SERVER/$BASE_PATH/$SUB_PATH?$PARAMS" $EXTRA -v \
	-H "Host: $HOST" > $DEBUG_OUTPUT
    curl -g "http://$SERVER/$BASE_PATH/$SUB_PATH?$PARAMS" $EXTRA -v \
	-H "Host: $HOST" 2> $DEBUG_OUTPUT
}

# test framework

COUNT=1

run()
{
    "$@" || { echo Failed to run "$@"; exit 1; }
}

expect()
{
    EXPECT="$1"
    RESULT="`${@:2}`"
    EXIT_VALUE=$?
    if [ $EXIT_VALUE != 0 ];then
        echo "Test #$COUNT ($2) failed.. command exited with $EXIT_VALUE"
        echo "Command: ${@:2}"
        exit 1
    fi

    if [ "x$RESULT" != "x$EXPECT" ];then
        echo "Test #$COUNT ($2) failed.."
        echo "Command: ${@:2}"
        echo "Expected: $1"
        echo "Got: $RESULT"
        exit 1
    else
        echo "Test #$COUNT ($2) succeeded"
    fi

    let COUNT=$COUNT+1
}

expectRegexp()
{
    EXPECT="$1"
    RESULT="`"${@:2}"`"
    EXIT_VALUE=$?
    if [ $EXIT_VALUE != 0 ];then
        echo "Test #$COUNT ($2) failed.. command exited with $EXIT_VALUE"
        echo "Command: ${@:2}"
        exit 1
    fi

    if echo $RESULT | egrep -e "$EXPECT" >& /dev/null;then
        echo "Test #$COUNT ($2) succeeded"
    else
        echo "Test #$COUNT ($2) failed.."
        echo "Command: ${@:2}"
        echo "Expected: $1"
        echo "Got: $RESULT"
        exit 1
    fi

    let COUNT=$COUNT+1
}

