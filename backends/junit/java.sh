#! /bin/sh

JAVA_HOME=/usr/lib/jvm/java-6-sun;	export JAVA_HOME
JVM_HOME=${JAVA_HOME};			export JVM_HOME
PATH=${JAVA_HOME}/bin:${PATH};		export PATH
INTERPRETER=${JAVA_HOME}/bin/java

ulimit -d `ulimit -H -d`
$INTERPRETER "$@"