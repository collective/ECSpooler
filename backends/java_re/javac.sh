#! /bin/sh

JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0; export JAVA_HOME
JVM_HOME=${JAVA_HOME}; export JVM_HOME
PATH=${JAVA_HOME}/Commands:${PATH}; export PATH

ulimit -d `ulimit -H -d`

${JAVA_HOME}/Commands/javac -encoding utf-8 "$@"
