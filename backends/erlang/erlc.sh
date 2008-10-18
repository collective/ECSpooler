#! /bin/ksh

ERL_HOME=${ERL_HOME:-"/opt/erlang/lib/erlang/erts-5.5.4"}

export ROOTDIR=${ERL_HOME%/*}
export BINDIR=${ERL_HOME}/bin

COMPILER=${BINDIR}/erlc

if [ -x /usr/bin/newtask -a -n "$USE_RCTL" ]; then
	pfexec /usr/bin/newtask -p Erlang -F $COMPILER "$@"
else
	$COMPILER "$@"
fi
