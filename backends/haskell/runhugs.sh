#! /bin/ksh

INTERPRETER=/local/usr/bin/runhugs
SELF="$0"
SLD="${SELF%/*}"
TLD=${SLD#/}
if [ "$SLD" = "$TLD" ]; then
	SLD=`pwd`/$SLD
fi
OPTIONS="-P{HUGS}:${SLD}/haskell_libs:"

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER $OPTIONS "$@" &

wait %%
