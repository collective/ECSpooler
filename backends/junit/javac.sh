#! /bin/sh

JAVA_HOME=${JAVA_HOME:-/System/Library/Frameworks/JavaVM.framework/Versions/1.5/Home}; export JAVA_HOME

JAVA_BIN=${JAVA_HOME}/bin
COMPILER=${JAVA_BIN}/javac

JVM_HOME=${JAVA_HOME}; export JVM_HOME
PATH=${JAVA_BIN}:${PATH}; export PATH

ulimit -d `ulimit -H -d`

${COMPILER} -encoding utf-8 "$@"
