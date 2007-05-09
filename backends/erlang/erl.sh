#! /bin/sh

PATH=$PATH:/opt/erlang/lib/erlang/erts-5.5.4/bin; export PATH
ROOTDIR=/opt/erlang/lib/erlang; export ROOTDIR
BINDIR=$ROOTDIR/erts-5.5.4/bin; export BINDIR

INTERPRETER="beam -- -root $ROOTDIR -progname erlexec -- -home /tmp -noshell -noinput -s $1 -s erlang halt"

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER &

wait %%
