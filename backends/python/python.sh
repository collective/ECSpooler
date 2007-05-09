#! /bin/sh

INTERPRETER=/usr/bin/python

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER "$@" &
wait %%
