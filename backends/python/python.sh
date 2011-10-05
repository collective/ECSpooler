#! /bin/sh

INTERPRETER=/opt/python-2.6/bin/python

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER "$@" &
wait %%
