#! /bin/sh

SELF="$0"
SLD="${SELF%/*}"
TLD=${SLD#/}
if [ "$SLD" = "$TLD" ]; then
    SLD=`pwd`/$SLD
fi

. $SLD/../javabase.sh

ulimit -d `ulimit -H -d`

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask ]; then
    /usr/bin/newtask -p Java -F $INTERPRETER ${JVM_OPTS} "$@" &
else
    $INTERPRETER ${JVM_OPTS} "$@" &
fi

wait %%
