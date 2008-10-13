#! /bin/ksh

INTERPRETER=${INTERPRETER:-/local/usr/bin/mzscheme}
OPTS="-m -v -f "

# Explanation of MzScheme command-line flags in use:
#
# -m
#    Suppresses the startup banner text.
#
# -v 
#    Don't load any startup scripts.
#
# -f file
#    Loads file after MzScheme starts.

trap 'kill $! && trap - TERM && kill $$' TERM

if [ -x /usr/bin/newtask ]; then
	/usr/bin/newtask -p Scheme -F $INTERPRETER $OPTIONS "$@" &
else
	$INTERPRETER $OPTIONS "$@" &
fi

wait %%
