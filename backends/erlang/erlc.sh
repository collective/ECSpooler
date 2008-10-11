#! /bin/ksh

ERL_HOME=${ERL_HOME:-"/opt/erlang/lib/erlang/erts-5.5.4"}

ROOTDIR=${ERL_HOME%/*}; export ROOTDIR
BINDIR=${ERL_HOME}/bin; export BINDIR

COMPILER=${BINDIR}/erlc

$COMPILER "$@"
