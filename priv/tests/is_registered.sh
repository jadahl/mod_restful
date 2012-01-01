#!/usr/bin/env bash

is_registered()
{
    USERNAME=$1

    get register/is_registered "username=$USERNAME"
}

[ $# = 1 ] && {
    CWD="$(dirname "$0")"
    . "$CWD/common.sh"

    is_registered "$@"
}

