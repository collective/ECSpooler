#! /bin/ksh

if [ -z "$JAVA_HOME" ]; then
	if [ -n "$INTERPRETER" ]; then
		JAVA_HOME=${INTERPRETER%/*}
	elif [ -n "$COMPILER" ]; then
		JAVA_HOME=${COMPILER%/*}
    elif [ `uname -s` = "Darwin" ]; then
        JBASE="/System/Library/Frameworks/JavaVM.framework/Versions"
        for d in "1.6/Home" "1.5/Home" "CurrentJDK/Home" ; do
            if [ -d "${JBASE}/$d" ]; then
                JAVA_HOME="${JBASE}/$d"
            fi
        done
    else
        for d in "jdk1.6.0_06" "jdk1.5.0_15" "latest"; do
            if [ -d "/usr/jdk/$d/bin" ]; then
                JAVA_HOME="/usr/jdk/$d"
            fi
        done
    fi
fi

JVM_OPTS="-d32 -client -Xms16M -Xmx64M"
export JAVA_HOME
JAVA_BIN=${JAVA_HOME}/bin
INTERPRETER=${JAVA_BIN}/java
COMPILER=${JAVA_BIN}/javac
export JVM_HOME=${JAVA_HOME}
export PATH=${JAVA_BIN}:${PATH}

