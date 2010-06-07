#! /bin/sh

INTERPRETER="java -cp /home/christian/workspace/ECSpooler/backends/xml/bin/saxon/saxon9he.jar net.sf.saxon.Query"

trap 'kill $! && trap - TERM && kill $$' TERM

# Call saxon:
$INTERPRETER "$@" &

wait %%