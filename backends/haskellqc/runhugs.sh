#! /bin/ksh

if [ -n "$HUGS_HOME" -a -x "${HUGS_HOME}/bin/runhugs" ]; then
	INTERPRETER="${HUGS_HOME}/bin/runhugs"
else
	INTERPRETER=${INTERPRETER:-/usr/local/bin/runhugs}
	if [ ! -x $INTERPRETER ]; then
		INTERPRETER=`whence runhugs 2>/dev/null`
	fi
fi
SELF="$0"
SLD="${SELF%/*}"
TLD=${SLD#/}
if [ "$SLD" = "$TLD" ]; then
        SLD=`pwd`/$SLD
fi
OPTS="-P{HUGS}:${SLD}/haskell_libs:"

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask -a -n "$USE_RCTL" ]; then
	/usr/bin/newtask -p Haskell -F $INTERPRETER $OPTS "$@" &
else
	$INTERPRETER $OPTS "$@" &
fi

wait %%
