#! /bin/ksh

ERL_HOME=${ERL_HOME:-"/opt/erlang/lib/erlang/erts-5.5.4"}

export ROOTDIR=${ERL_HOME%/*}
export BINDIR=${ERL_HOME}/bin
export PATH=${BINDIR}:${PATH}

INTERPRETER="beam -- -root $ROOTDIR -progname erlexec -- \
	-home /tmp -noshell -noinput -s $1 -s erlang halt"

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask -a -n "$USE_RCTL" ]; then
	/usr/bin/pfexec /usr/bin/newtask -p Erlang -F $INTERPRETER &
else
	$INTERPRETER &
fi
wait %%
