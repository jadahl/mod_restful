#!/usr/bin/env bash

USER=t1
PASSWORD=hejhej
ALT_PASSWORD=sansan
EMAIL=t1@test.test

ADMIN_USER=ta1

CWD="$(dirname "$0")"
. "$CWD/common.sh"

. "$CWD/is_registered.sh"
. "$CWD/unregister.sh"
. "$CWD/register.sh"
. "$CWD/change_password.sh"
. "$CWD/force_change_password.sh"
. "$CWD/admin.sh"

if [ "x`is_registered $USER`" = "xtrue" ];then
    echo Unregistering $USER...
    expect '"ok"' unregister "$USER" "$PASSWORD"
fi

# test is_registered false
expect false is_registered "$USER"

# register
expect "\"ok\"" register "$USER" "$PASSWORD" "$EMAIL"

# test is_registered true
expect true is_registered "$USER"

# unregister
expect "\"ok\"" unregister "$USER" "$PASSWORD"

# test is_registered false
expect false is_registered "$USER"

# register
expect "\"ok\"" register "$USER" "$PASSWORD" "$EMAIL"

# test change_password
expect '"ok"' change_password "$USER" "$PASSWORD" sansan
expect '"ok"' change_password "$USER" sansan "$PASSWORD"

# test admin
if [ "x`is_registered $ADMIN_USER`" = "xtrue" ];then
    echo Unregistering $ADMIN_USER...
    expect '"ok"' unregister "$ADMIN_USER" "$PASSWORD"
fi
expectRegexp '\{"ok":.*\}' admin register "[\"$ADMIN_USER\", \"$HOST\", \"$PASSWORD\"]"
expectRegexp '\{"ok":.*\}' admin unregister "[\"$ADMIN_USER\", \"$HOST\"]"

# test force_change_password
expect '"ok"' force_change_password "$USER" sansan
expect '"ok"' force_change_password "$USER" "$PASSWORD"

