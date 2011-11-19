#!/usr/bin/env bash

admin()
{
    JSON=`cat << __EOF__
{
    "key":"$KEY",
    "host":"$HOST",
    "command":"$1",
    "args":$2
}
__EOF__`
    request admin "$JSON"
}

[ $# = 2 ] && {
    CWD="$(dirname "$0")"
    . "$CWD/common.sh"

    echo Changing password...
    
    admin "$@"
}

