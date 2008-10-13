#! /bin/ksh

if [ -n "$PYTHONHOME" -a -x "${PYTHONHOME}/bin/python" ]; then
	INTERPRETER="${PYTHONHOME}/bin/python"
else
	INTERPRETER=${INTERPRETER:-python}
fi

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask -a -n "$USE_RCTL" ]; then
	/usr/bin/newtask -p Python -F $INTERPRETER "$@" &
else
	$INTERPRETER "$@" &
fi
wait %%
