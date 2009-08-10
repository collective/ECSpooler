#! /bin/sh

INTERPRETER=/Library/Frameworks/Python.framework/Versions/Current/bin/python

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER "$@" &
wait %%
