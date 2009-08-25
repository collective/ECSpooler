#! /bin/sh

INTERPRETER=/opt/hugs/bin/runhugs
OPTIONS="-P{HUGS}:/opt/ECSpooler/backends/haskell/haskell_libs:"

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER $OPTIONS "$@" &

wait %%
