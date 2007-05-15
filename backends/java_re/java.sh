#! /bin/sh

JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0; export JAVA_HOME
JVM_HOME=${JAVA_HOME}; export JVM_HOME

PATH=${JAVA_HOME}/Commands:${PATH}; export PATH

INTERPRETER=${JAVA_HOME}/Commands/java
OPTIONS=-Dfile.encoding="utf-8"

ulimit -d `ulimit -H -d`

trap 'kill $! && trap - TERM && kill $$' TERM

$INTERPRETER $OPTIONS "$@" &

wait %%
