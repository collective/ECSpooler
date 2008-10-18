#! /bin/ksh

if [ -n "$CL_HOME" -a -x "${CL_HOME}/bin/sbcl" ]; then
	INTERPRETER="${CL_HOME}/bin/sbcl"
else
	INTERPRETER=${INTERPRETER:-"/opt/sbcl-1.0.5/bin/sbcl"}
fi
export SBCL_HOME=${INTERPRETER%/bin/sbcl}/lib/sbcl
OPTS=" --noinform --disable-debugger --no-sysinit --no-userinit "

# Explanation of SBCL command-line flags in use:
#
# --disable-debugger
#    By default, a Common Lisp system tries to ask the programmer  for
#    help when it gets in trouble (by printing a debug prompt, then
#    listening, on *DEBUG-IO*). However, this is not useful behavior
#    for  a  system running  with  no programmer available, and this
#    option tries to set up more appropriate behavior for that
#    situation
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

if [ -x /usr/bin/newtask -a -n "$USE_RCTL" ]; then
	pfexec /usr/bin/newtask -p CommonLisp -F $INTERPRETER $OPTS --load "$@" --eval "(quit)" &
else
	$INTERPRETER $OPTS --load "$@" --eval "(quit)" &
fi

wait %%
