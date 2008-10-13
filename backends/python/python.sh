#! /bin/ksh

INTERPRETER=${INTERPRETER:-python}

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask ]; then
	/usr/bin/newtask -p Python -F $INTERPRETER "$@" &
else
	$INTERPRETER "$@" &
fi
wait %%
