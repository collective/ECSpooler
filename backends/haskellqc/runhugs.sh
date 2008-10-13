#! /bin/ksh

INTERPRETER=${INTERPRETER:-/usr/local/bin/runhugs}
if [ ! -x $INTERPRETER ]; then
	INTERPRETER=`whence runhugs 2>/dev/null`
fi
SELF="$0"
SLD="${SELF%/*}"
TLD=${SLD#/}
if [ "$SLD" = "$TLD" ]; then
        SLD=`pwd`/$SLD
fi
OPTIONS="-P{HUGS}:${SLD}/haskell_libs:"

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask ]; then
	/usr/bin/newtask -p Haskell -F $INTERPRETER $OPTIONS "$@" &
else
	$INTERPRETER $OPTIONS "$@" &
fi

wait %%
