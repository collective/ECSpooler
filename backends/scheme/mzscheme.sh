#! /bin/sh

INTERPRETER=/opt/mzscheme/bin/mzscheme
OPTIONS="-m -v -f "

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

$INTERPRETER $OPTIONS "$@" &

wait %%
