#! /bin/ksh

if [ -n "$SCHEME_HOME" -a -x "${SCHEME_HOME}/bin/mzscheme" ]; then
	INTERPRETER="${SCHEME_HOME}/bin/mzscheme"
else
	INTERPRETER=${INTERPRETER:-/usr/local/bin/mzscheme}
fi
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

if [ -x /usr/bin/newtask -a -n "$USE_RCTL" ]; then
	/usr/bin/pfexec /usr/bin/newtask -p Scheme -F $INTERPRETER $OPTS "$@" &
else
	$INTERPRETER $OPTS "$@" &
fi

wait %%
