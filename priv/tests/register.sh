#!/usr/bin/env bash

register()
{
    USERNAME=$1
    PASSWORD=$2
    NEW_EMAIL=$3

    JSON=`cat << __EOF__
{
    "key":"$KEY",
    "username":"$USERNAME",
    "host":"$HOST",
    "password":"$PASSWORD"
__EOF__`

    if [ $# -eq 3 ]; then
        JSON="$JSON,\"email\":\"$NEW_EMAIL\""
    fi

    JSON="$JSON\
    }"

    request register/register "$JSON"
}

[ $# = 3 ] && {
    CWD="$(dirname "$0")"
    . "$CWD/common.sh"

    register "$@"
}
