#!/usr/bin/env bash

change_password()
{
    USERNAME="$1"
    OLD_PASSWORD="$2"
    NEW_PASSWORD="$3"

    JSON=`cat << __EOF__
{
    "key":"$KEY",
    "username":"$USERNAME",
    "host":"$HOST",
    "old_password":"$OLD_PASSWORD",
    "new_password":"$NEW_PASSWORD"
}
__EOF__`
    request register/change_password "$JSON"
}

[ $# = 3 ] && {
    CWD="$(dirname "$0")"
    . "$CWD/common.sh"

    echo Changing password...
    
    change_password "$@"
}

