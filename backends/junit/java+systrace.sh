#! /opt/ast/bin/ksh

JAVA_HOME=/usr/pkg/java/sun-1.5;        export JAVA_HOME
JVM_HOME=${JAVA_HOME};			export JVM_HOME
PATH=${JAVA_HOME}/bin:${PATH};		export PATH
INTERPRETER=${JAVA_HOME}/bin/java

POLICYDIR=/opt/ECSpooler/systrace-policy
POLICYFILE=${INTERPRETER//[\/.-]/_}
POLICYFILE=${POLICYFILE/_}

if [[ -r "$POLICYDIR/$POLICYFILE" ]]; then
   ulimit -d `ulimit -H -d`

   trap 'kill $! && trap - TERM && kill $$' TERM

   systrace -a -d $POLICYDIR $INTERPRETER -classpath ".:./junit_libs/algds.jar:./junit_libs/junit-4.3.1.jar" "$@" &
   wait %%
else
   print "Internal error: Policy file not readable.  Please contact your tutor."
   exit 1
fi
