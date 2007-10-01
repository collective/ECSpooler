#! /bin/sh

JAVA_HOME=/usr/lib/jvm/java-6-sun;	export JAVA_HOME
JVM_HOME=${JAVA_HOME};			export JVM_HOME
PATH=${JAVA_HOME}/bin:${PATH};		export PATH

ulimit -d `ulimit -H -d`
${JAVA_HOME}/bin/javac -classpath ".:./junit/junit_libs/algds.jar:./junit_libs/junit-4.3.1.jar" -encoding utf-8 "$@"
