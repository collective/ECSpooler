#! /bin/ksh

SELF="$0"
SLD="${SELF%/*}"
TLD=${SLD#/}
if [ "$SLD" = "$TLD" ]; then
    SLD=`pwd`/$SLD
fi

. $SLD/../javabase.sh

ulimit -d `ulimit -H -d`

if [ -x /usr/bin/newtask -a -n "$USE_RCTL" ]; then
	/usr/bin/newtask -p Java -F ${COMPILER} -encoding utf-8 "$@"
else
	${COMPILER} -encoding utf-8 "$@"
fi
