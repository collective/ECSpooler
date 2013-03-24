#! /bin/sh

INTERPRETER=/usr/bin/runghc
#OPTIONS="-P{HUGS}:/opt/ECSpooler/backends/haskell/haskell_libs:"

trap 'kill $! && trap - TERM && kill $$' TERM

#$INTERPRETER $OPTIONS "$@" &
$INTERPRETER "$@" &

wait %%
