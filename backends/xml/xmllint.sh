#! /bin/sh

INTERPRETER=$(which xmllint)

trap 'kill $! && trap - TERM && kill $$' TERM

# By default, xmllint will output the resulting tree. We are not interested in it, so we specify
# --noout (cf. `man xmllint`)
$INTERPRETER --noout "$@" &

wait %%
