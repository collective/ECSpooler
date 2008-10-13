#! /bin/ksh

ERL_HOME=${ERL_HOME:-"/opt/erlang/lib/erlang/erts-5.5.4"}
if [ ! -e "${ERL_HOME}" ]; then
    if [ -n "$INTERPRETER" ]; then
        # TODO: resolve links
        ERL_HOME=`cd ${INTERPRETER%/*}; pwd`
        ERL_HOME=${ERL_HOME%/*}
    else
        COMPILER=`whence erlc 2>/dev/null`
        if [ -n "$INTERPRETER" ]; then
            ERL_HOME=`cd ${INTERPRETER%/*}; pwd`
            ERL_HOME=${ERL_HOME%/*}
        fi
    fi
fi

export ROOTDIR=${ERL_HOME%/*}
export BINDIR=${ERL_HOME}/bin

INTERPRETER="beam -- -root $ROOTDIR -progname erlexec -- -home /tmp -noshell -noinput -s $1 -s erlang halt"

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask ]; then
	/usr/bin/newtask -p Erlang -F $INTERPRETER &
else
	$INTERPRETER &
fi
wait %%
