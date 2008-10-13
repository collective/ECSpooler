#! /bin/ksh

# The problem with the SWI Prolog interpreter is that it doesn't exit
# with an error if a syntax error is encountered.  So we execute the
# script we were handed, collect all STDOUT and STDERR output in $OP
# and exit with an error ourselves if the output from the interpreter
# contains a line that starts with "ERROR".

# Explanation of the command line options in use:
#
# -q
#     Be quiet.
#
# -f none
#     Don't load any startup scripts.
#
# -t halt
#     Halt the interpreter after the script has been executed
#     instead of switching to the top-level.
#
# -s script
#    Execute this script.

INTERPRETER=${INTERPRETER:-"/opt/swiprolog/bin/pl"}
if [ -x /usr/bin/newtask ]; then
	OP=$(/usr/bin/newtask -p Prolog -F ${INTERPRETER} -q -f none -t halt -s "$@" 2>&1)
else
	OP=$(${INTERPRETER} -q -f none -t halt -s "$@" 2>&1)
fi
ERR=$?

echo "$OP"

if [ $ERR -ne 0 ]
then
  # The interpreter's exit code is != 0.  Fine, exit with this one.
  exit $ERR
else
  # Exit with an error if the interpreter's output contains a line
  # that starts with "ERROR" or if it contains a line that says "Goal
  # (directive) failed".
  if echo "$OP" | grep -q '^ERROR\|Goal (directive) failed:'
  then
    exit 1
  else
    exit 0
  fi
fi
