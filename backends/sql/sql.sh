#! /bin/sh

JAVA_HOME=/System/Library/Java/JavaVirtualMachines/1.6.0.jdk/Contents/Home; export JAVA_HOME
JAVA_BIN=${JAVA_HOME}/bin
JAVA=${JAVA_BIN}/java

JVM_HOME=${JAVA_HOME}; export JVM_HOME
PATH=${JAVA_BIN}:${PATH}; export PATH

LIB_PATH=/opt/ECSpooler/backends/sql/lib

ulimit -d `ulimit -H -d`

trap 'kill $! && trap - TERM && kill $$' TERM

$JAVA -cp $LIB_PATH/dbunit-2.4.8.jar:$LIB_PATH/SQLAssessment.jar:$LIB_PATH/hsqldb.jar:$LIB_PATH/slf4j-api-1.7.2.jar:$LIB_PATH/slf4j-simple-1.7.2.jar:$LIB_PATH/commons-logging-1.1.1.jar SQLAssessment.Main &

wait %%
