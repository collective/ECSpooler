#! /bin/ksh

ERL_HOME=${ERL_HOME:-"/opt/erlang/lib/erlang/erts-5.5.4"}
if [ ! -e "${ERL_HOME}" ]; then
	if [ -n "$COMPILER" ]; then
		# TODO: resolve links
		ERL_HOME=`cd ${COMPILER%/*}; pwd`
		ERL_HOME=${ERL_HOME%/*}
	else
		COMPILER=`whence erlc 2>/dev/null`
		if [ -n "$COMPILER" ]; then
			ERL_HOME=`cd ${COMPILER%/*}; pwd`
			ERL_HOME=${ERL_HOME%/*}
		fi
	fi
fi

export ROOTDIR=${ERL_HOME%/*}
export BINDIR=${ERL_HOME}/bin

COMPILER=${BINDIR}/erlc

if [ -x /usr/bin/newtask ]; then
	/usr/bin/newtask -p Erlang -F $COMPILER "$@"
else
	$COMPILER "$@"
fi
