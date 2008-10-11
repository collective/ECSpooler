#! /bin/sh

INTERPRETER=python

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER "$@" &
wait %%
