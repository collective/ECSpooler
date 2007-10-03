#! /bin/sh

JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.5/Home; export JAVA_HOME

JAVA_BIN=${JAVA_HOME}/bin
INTERPRETER=${JAVA_BIN}/java

JVM_HOME=${JAVA_HOME}; export JVM_HOME
PATH=${JAVA_BIN}:${PATH}; export PATH

ulimit -d `ulimit -H -d`

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER "$@" &

wait %%
