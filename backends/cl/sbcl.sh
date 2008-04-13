#! /bin/sh

INTERPRETER=/opt/sbcl-1.0.5/bin/sbcl
SBCL_HOME=${INTERPRETER%/bin/sbcl}/lib/sbcl; export SBCL_HOME
OPTIONS="--noinform --no-sysinit --no-userinit "

# Explanation of SBCL command-line flags in use:
#
# --noinform
#    Suppresses the startup banner text.
#
# --no-sysinit
#    Don't load any system-wide initialization files.
#
# --no-userinit
#    Don't load any user initialization file.
#
# --load file
#    Loads file after SBCL starts.
#
# --eval "(quit)"
#    Quit after executing the loaded file.

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER $OPTIONS --load "$@" --eval "(quit)" &

wait %%
