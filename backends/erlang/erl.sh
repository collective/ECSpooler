#! /bin/ksh

ERL_HOME=${ERL_HOME:-"/opt/erlang/lib/erlang/erts-5.5.4"}

ROOTDIR=${ERL_HOME%/*}; export ROOTDIR
PATH=$PATH:${ERL_HOME}/bin; export PATH
BINDIR=${ERL_HOME}/bin; export BINDIR

INTERPRETER="beam -- -root $ROOTDIR -progname erlexec -- -home /tmp -noshell -noinput -s $1 -s erlang halt"

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER &

wait %%
