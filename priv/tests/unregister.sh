#!/usr/bin/env bash

unregister()
{
    USERNAME=$1
    PASSWORD=$2

    JSON=`cat << __EOF__
{
    "key":"$KEY",
    "username":"$USERNAME",
    "host":"$HOST",
    "password":"$PASSWORD"
}
__EOF__`
    
    request register/unregister "$JSON"
}

[ $# = 2 ] && {
    CWD="$(dirname "$0")"
    . "$CWD/common.sh"

    unregister "$@"
}

