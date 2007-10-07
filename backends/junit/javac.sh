#! /bin/sh

JAVA_HOME=/usr/lib/jvm/java-6-sun;	export JAVA_HOME
JVM_HOME=${JAVA_HOME};			export JVM_HOME
PATH=${JAVA_HOME}/bin:${PATH};		export PATH

ulimit -d `ulimit -H -d`
${JAVA_HOME}/bin/javac -encoding utf-8 "$@"